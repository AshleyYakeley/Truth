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
{-
    type Subscription edit action = forall editor userstate.
        userstate ->
        (Object edit userstate -> IO (editor,userstate)) -> -- initialise: provides read MutableEdit, initial allowed, write MutableEdit
        (forall m. MonadIOInvert m => editor -> MutableRead m (EditReader edit) -> userstate -> [edit] -> m userstate) -> -- receive: get updates (both others and from your mutableEdit calls)
        IO (editor, action);

    newtype Object edit userstate = MkObject (forall r. (forall m. MonadIOInvert m => userstate -> MutableEdit m edit userstate -> m r) -> IO r);
-}
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
                objB = MkObject $ \ff -> objA $ \(sB,MkSaveBuffer buffer _) _ -> let
                {
                    mutableRead :: MutableRead m (WholeReader a);
                    mutableRead = undefined;

                    mutableEdit :: [WholeEdit a] -> m (Maybe (m stateB));
                    mutableEdit = undefined;

                    mutedB :: MutableEdit m (WholeEdit a) stateB;
                    mutedB = MkMutableEdit{..};
                } in ff sB mutedB;
            };
            (edB,usB) <- initB objB;
            let
            {
                saveAction :: IO Bool;
                saveAction = objA $ \(_oldstateB,MkSaveBuffer buffer _) muted -> do
                {
                    maction <- mutableEdit muted [MkWholeEdit buffer];
                    case maction of
                    {
                        Nothing -> return False;
                        Just action -> do
                        {
                            action;
                            return True;
                        }
                    }
                };

                revertAction :: IO Bool;
                revertAction = objA $ \(oldstateB,MkSaveBuffer _ _) muted -> do
                {
                    buffer <- mutableRead muted ReadWhole;
                    newstateB <- receiveB edB (\ReadWhole -> return buffer) oldstateB [MkWholeEdit buffer];
                    return False; -- MkSaveBuffer buffer False
                };

                saveActions :: SaveActions;
                saveActions = objA $ \(_,MkSaveBuffer _ changed) _ -> return $ if changed then Just (saveAction,revertAction) else Nothing;

                edA = (edB,saveActions);
            };
            buffer <- objA $ \_ muted -> mutableRead muted ReadWhole;
            let
            {
                usA :: (stateB,SaveBuffer a);
                usA = (usB,MkSaveBuffer buffer False);
            };
            return (edA,usA);
        };

        receiveA :: forall m. MonadIOInvert m => (editorB,SaveActions) -> MutableRead m (WholeReader a) -> (stateB,SaveBuffer a) -> [WholeEdit a] -> m (stateB,SaveBuffer a);
        receiveA _ _ state@(_,MkSaveBuffer _ True) _ = return state;
        receiveA (edB,_) _ (oldstateB,MkSaveBuffer oldbuffer False) edits = do
        {
            let
            {
                newbuffer = fromReadFunction (applyEdits edits) oldbuffer;
            };
            newstateB <- receiveB edB (readFromM $ return newbuffer) oldstateB edits;
            return (newstateB,MkSaveBuffer newbuffer False);
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
