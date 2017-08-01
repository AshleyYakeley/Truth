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

    saveBufferMutableEdit :: forall m edit. (PureFullEdit edit,Monad m) => ([edit] -> StateT (SaveBuffer (EditSubject edit)) m ()) -> MutableEdit (StateT (SaveBuffer (EditSubject edit)) m) edit;
    saveBufferMutableEdit update = let
    {
        mutableRead :: MutableRead (StateT (SaveBuffer (EditSubject edit)) m) (EditReader edit);
        mutableRead = readFromM $ fmap saveBuffer get;

        mutableEdit :: [edit] -> StateT (SaveBuffer (EditSubject edit)) m (Maybe (StateT (SaveBuffer (EditSubject edit)) m ()));
        mutableEdit edits = return $ Just $ do
        {
            MkSaveBuffer oldbuf _ <- get;
            put $ MkSaveBuffer (fromReadFunction (applyEdits edits) oldbuf) True;
            update edits;
        };
    } in MkMutableEdit{..};

    newtype SaveActions = MkSaveActions (IO (Maybe (IO Bool,IO Bool)));

    saveBufferSubscriber :: forall edit action. PureFullEdit edit => Subscriber (WholeEdit (EditSubject edit)) action -> Subscriber edit (action,SaveActions);
    saveBufferSubscriber subA = MkSubscriber $ \(initB :: Object edit -> IO editorB) updateB -> do
    {
        sbVar <- newMVar $ error "uninitialised save buffer";
        let
        {
            initA :: Object (WholeEdit (EditSubject edit)) -> IO (editorB,SaveActions);
            initA (MkObject objA) = do
            {
                firstBuf <- objA $ \muted -> mutableRead muted ReadWhole;
                mvarStateAccess sbVar $ put $ MkSaveBuffer firstBuf False;

                rec
                {
                    let
                    {
                        objB :: Object edit;
                        objB = MkObject $ \call -> mvarStateAccess sbVar $ call $ saveBufferMutableEdit $ \edits -> updateB edB (readFromM $ fmap saveBuffer get) edits;
                    };
                    edB <- initB objB;
                };
                let
                {
                    saveAction :: IO Bool;
                    saveAction = objA $ \muted -> do
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
                    revertAction = objA $ \muted -> do
                    {
                        buf <- mutableRead muted ReadWhole;
                        mvarStateAccess sbVar $ put $ MkSaveBuffer buf False;
                        updateB edB (readFromM $ return buf) $ getReplaceEdits buf;
                        return False;
                    };

                    saveActions :: SaveActions;
                    saveActions = MkSaveActions $ objA $ \_ -> do
                    {
                        MkSaveBuffer _ changed <- mvarStateAccess sbVar get;
                        return $ if changed then Just (saveAction,revertAction) else Nothing;
                    };

                    edA = (edB,saveActions);
                };
                return edA;
            };

            updateA :: forall m. IsStateIO m => (editorB,SaveActions) -> MutableRead m (WholeReader (EditSubject edit)) -> [WholeEdit (EditSubject edit)] -> m ();
            updateA (edB,_) _ edits = do
            {
                MkSaveBuffer oldbuffer changed <- mvarStateAccess sbVar get;
                if changed then return () else do
                {
                    let
                    {
                        newbuffer = fromReadFunction (applyEdits edits) oldbuffer;
                    };
                    updateB edB (readFromM $ return newbuffer) $ getReplaceEdits newbuffer;
                    mvarStateAccess sbVar $ put $ MkSaveBuffer newbuffer False;
                };
            };
        };
        (edA,closerA,actionA) <- subscribe subA initA updateA;
        let
        {
            (edB,saveActions) = edA;
            actionB = (actionA,saveActions);
            closerB = closerA; -- add UI query here
        };
        return (edB,closerB,actionB);
    };
}
