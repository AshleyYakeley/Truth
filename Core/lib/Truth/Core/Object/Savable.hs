module Truth.Core.Object.Savable (SaveActions(..),saveBufferSubscriber) where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Whole;
    import Truth.Core.Object.MutableEdit;
    import Truth.Core.Object.Object;
    import Truth.Core.Object.Subscriber;


    data SaveBuffer a = MkSaveBuffer
    {
        saveBuffer :: a,
        _saveBufferChanged :: Bool
    };

    saveBufferMutableEdit :: forall m a. Monad m => MutableEdit (StateT (SaveBuffer a) m) (WholeEdit a);
    saveBufferMutableEdit = let
    {
        mutableRead :: MutableRead (StateT (SaveBuffer a) m) (WholeReader a);
        mutableRead ReadWhole = fmap saveBuffer get;

        mutableEdit :: [WholeEdit a] -> (StateT (SaveBuffer a) m) (Maybe (StateT (SaveBuffer a) m ()));
        mutableEdit = singleMutableEdit $ \(MkWholeEdit a) -> put $ MkSaveBuffer a True;
    } in MkMutableEdit{..};

    newtype SaveActions = MkSaveActions (IO (Maybe (IO Bool,IO Bool)));

    saveBufferSubscriber :: forall a action. Subscriber (WholeEdit a) action -> Subscriber (WholeEdit a) (action,SaveActions);
    saveBufferSubscriber subA (fsB :: stateB) (initB :: Object (WholeEdit a) stateB -> IO editorB) receiveB = do
    {
        sbVar <- newMVar $ error "uninitialised save buffer";
        let
        {
            fsA :: stateB;
            fsA = fsB;

            initA :: Object (WholeEdit a) stateB -> IO (editorB,SaveActions);
            initA (MkObject objA) = do
            {
                objA $ \muted _acc -> do
                {
                    buf <- mutableRead muted ReadWhole;
                    mvarStateAccess sbVar $ put $ MkSaveBuffer buf False;
                };
                let
                {
                    objB :: Object (WholeEdit a) stateB;
                    objB = MkObject $ \call -> objA $ \_mutedA accA -> mvarStateAccess sbVar $ call saveBufferMutableEdit $ liftStateAccess accA;
                };
                edB <- initB objB;
                let
                {
                    saveAction :: IO Bool;
                    saveAction = objA $ \muted _acc -> do
                    {
                        MkSaveBuffer buf _ <- mvarStateAccess sbVar get;
                        maction <- mutableEdit muted [MkWholeEdit buf];
                        case maction of
                        {
                            Nothing -> return False;
                            Just action -> do
                            {
                                action;
                                mvarStateAccess sbVar $ put $ MkSaveBuffer buf False;
                                return True;
                            }
                        }
                    };

                    revertAction :: IO Bool;
                    revertAction = objA $ \muted acc -> do
                    {
                        buf <- mutableRead muted ReadWhole;
                        mvarStateAccess sbVar $ put $ MkSaveBuffer buf False;
                        acc $ receiveB edB (\ReadWhole -> return buf) [MkWholeEdit buf];
                        return False;
                    };

                    saveActions :: SaveActions;
                    saveActions = MkSaveActions $ objA $ \_ _acc -> do
                    {
                        MkSaveBuffer _ changed <- mvarStateAccess sbVar get;
                        return $ if changed then Just (saveAction,revertAction) else Nothing;
                    };

                    edA = (edB,saveActions);
                };
                return edA;
            };

            receiveA :: forall m. IsStateIO m => (editorB,SaveActions) -> MutableRead m (WholeReader a) -> [WholeEdit a] -> StateT stateB m ();
            receiveA (edB,_) _ edits = do
            {
                MkSaveBuffer oldbuffer changed <- mvarStateAccess sbVar get;
                if changed then return () else do
                {
                    let
                    {
                        newbuffer = fromReadFunction (applyEdits edits) oldbuffer;
                    };
                    receiveB edB (readFromM $ return newbuffer) edits;
                    mvarStateAccess sbVar $ put $ MkSaveBuffer newbuffer False;
                };
            };
        };
        (edA,closerA,actionA) <- subA fsA initA receiveA;
        let
        {
            (edB,saveActions) = edA;
            actionB = (actionA,saveActions);
            closerB = closerA; -- add UI query here
        };
        return (edB,closerB,actionB);
    };
}
