module Truth.Core.Object.Savable where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Whole;
    import Truth.Core.Object.MutableEdit;
    import Truth.Core.Object.Object;
    import Truth.Core.Object.Subscription;


    data SaveBuffer a = MkSaveBuffer
    {
        saveBuffer :: a,
        saveBufferChanged :: Bool
    };

    type SaveActions = IO (Maybe (IO Bool,IO Bool));

    saveBufferSubscription :: forall a action. Subscription (WholeEdit a) action -> Subscription (WholeEdit a) (action,SaveActions);
    saveBufferSubscription subA (fsB :: stateB) (initB :: Object (WholeEdit a) stateB -> IO (editorB,stateB)) receiveB = let
    {
        fsA :: (stateB,SaveBuffer a);
        fsA = (fsB,error "uninitialised save buffer");

        initA :: Object (WholeEdit a) (stateB,SaveBuffer a) -> IO ((editorB,SaveActions),(stateB,SaveBuffer a));
        initA (MkObject objA) = do
        {
            let
            {
                objB :: Object (WholeEdit a) stateB;
                objB = MkObject $ \ff -> objA $ \_mutedA -> let
                {
                    mutableRead :: MutableRead m (WholeReader a);
                    mutableRead = undefined;

                    mutableEdit :: [WholeEdit a] -> m (Maybe (m stateB));
                    mutableEdit = undefined;

                    mutedB :: MutableEdit m (WholeEdit a) stateB;
                    mutedB = MkMutableEdit{..};
                } in stateFst $ ff mutedB;
            };
            (edB,usB) <- initB objB;
            let
            {
                saveAction :: IO Bool;
                saveAction = objA $ \muted -> do
                {
                    (_oldstateB,MkSaveBuffer buffer _) <- get;
                    maction <- lift $ mutableEdit muted [MkWholeEdit buffer];
                    case maction of
                    {
                        Nothing -> return False;
                        Just action -> do
                        {
                            _ <- lift action;
                            return True;
                        }
                    }
                };

                revertAction :: IO Bool;
                revertAction = objA $ \muted -> do
                {

                    buffer <- lift $ mutableRead muted ReadWhole;
                    stateFst $ receiveB edB (\ReadWhole -> return buffer) [MkWholeEdit buffer];
                    return False; -- MkSaveBuffer buffer False
                };

                saveActions :: SaveActions;
                saveActions = objA $ \_ -> do
                {
                    (_,MkSaveBuffer _ changed) <- get;
                    return $ if changed then Just (saveAction,revertAction) else Nothing;
                };

                edA = (edB,saveActions);
            };
            buffer <- objA $ \muted -> lift $ mutableRead muted ReadWhole;
            let
            {
                usA :: (stateB,SaveBuffer a);
                usA = (usB,MkSaveBuffer buffer False);
            };
            return (edA,usA);
        };

        receiveA :: forall m. IsStateIO m => (editorB,SaveActions) -> MutableRead m (WholeReader a) -> [WholeEdit a] -> StateT (stateB,SaveBuffer a) m ();
        receiveA (edB,_) _ edits = do
        {
            MkSaveBuffer oldbuffer changed <- stateSnd get;
            if changed then return () else do
            {
                let
                {
                    newbuffer = fromReadFunction (applyEdits edits) oldbuffer;
                };
                stateFst $ receiveB edB (readFromM $ return newbuffer) edits;
                stateSnd $ put $ MkSaveBuffer newbuffer False;
            };
        };

        resultB :: IO (editorB,(action,SaveActions));
        resultB = do
        {
            (edA,actionA) <- subA fsA initA receiveA;
            let
            {
                (edB,saveActions) = edA;
                actionB = (actionA,saveActions);
            };
            return (edB,actionB);
        };
    } in resultB;

{-
    saveBufferLens :: forall a. (EditReader (SaveAction a) ~ WholeReader a) =>
        FloatingEditLens (SaveBuffer a) (WholeEdit a) (SaveAction a);
    saveBufferLens = let
    {
        floatingEditInitial :: SaveBuffer a;
        floatingEditInitial = MkSaveBuffer undefined False;

        floatingEditGet :: SaveBuffer a -> ReadFunction (WholeReader a) (WholeReader a);
        floatingEditGet (MkSaveBuffer buffer _) ReadWhole = return buffer;

        floatingEditUpdate :: WholeEdit a -> SaveBuffer a -> Readable (WholeReader a) (SaveBuffer a,[SaveAction a]);
        floatingEditUpdate (MkWholeEdit buffer) _ = return (MkSaveBuffer buffer False,[SAChange buffer]);

        floatingEditLensFunction :: FloatingEditFunction (SaveBuffer a) (WholeEdit a) (SaveAction a);
        floatingEditLensFunction = MkFloatingEditFunction{..};

        floatingEditLensPutEdit :: SaveBuffer a -> SaveAction a -> Readable (WholeReader a) (Maybe (SaveBuffer a,[WholeEdit a]));
        floatingEditLensPutEdit (MkSaveBuffer _ False) SASave = return Nothing;
        floatingEditLensPutEdit (MkSaveBuffer buffer True) SASave = return $ Just (MkSaveBuffer buffer False,[MkWholeEdit buffer]);
        floatingEditLensPutEdit (MkSaveBuffer _ False) SARevert = return Nothing;
        floatingEditLensPutEdit (MkSaveBuffer _ True) SARevert = do
        {
            buffer <- readable ReadWhole;
            return $ Just (MkSaveBuffer buffer False,[]);
        };
        floatingEditLensPutEdit (MkSaveBuffer _ _) (SAChange buffer) = return $ Just (MkSaveBuffer buffer True,[]);
    } in MkFloatingEditLens{..};
-}
}
