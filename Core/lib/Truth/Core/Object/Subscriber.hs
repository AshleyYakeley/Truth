module Truth.Core.Object.Subscriber
(
    Subscriber,SubscriberW(..),makeObjectSubscriber,
    liftIO,
    objectSubscriber,makeSharedSubscriber,
) where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Object.MutableEdit;
    import Truth.Core.Object.Object;


    type Subscriber edit actions = forall editor userstate.
        userstate ->
        (Object edit userstate -> IO editor) -> -- initialise: provides read MutableEdit, initial allowed, write MutableEdit
        (forall m. IsStateIO m => editor -> MutableRead m (EditReader edit) -> [edit] -> StateT userstate m ()) -> -- receive: get updates (both others and from your mutableEdit calls)
        IO (editor,IO (),actions);

    newtype SubscriberW edit a = MkSubscriberW (Subscriber edit a);

    instance Functor (SubscriberW edit) where
    {
        fmap ab (MkSubscriberW sub) = MkSubscriberW $ \fs initialise receive -> do
        {
            (editor,cl,a) <- sub fs initialise receive;
            return (editor,cl,ab a);
        };
    };

    data UpdateStoreEntry edit userstate = MkStoreEntry (forall m. IsStateIO m => MutableRead m (EditReader edit) -> [edit] -> StateT userstate m ()) userstate;

    useStateLens :: Lens' Identity (UpdateStoreEntry edit userstate) userstate;
    useStateLens = let
    {
        lensGet (MkStoreEntry _ s) = s;
        lensPutback s (MkStoreEntry u _) = Identity $ MkStoreEntry u s;
    } in MkLens{..};

    type UpdateStore edit = IOWitnessStore (UpdateStoreEntry edit);

    runUpdateStoreEntry :: IsStateIO m => ((MutableRead m (EditReader edit) -> [edit] -> StateT userstate m ()) -> StateT userstate m ()) -> StateT (UpdateStoreEntry edit userstate) m userstate;
    runUpdateStoreEntry call = do
    {
        MkStoreEntry update _ <- get;
        lensStateT useStateLens $ do
        {
            call update;
            get;
        };
    };

    runUpdateStore :: IsStateIO m => (forall userstate. IOWitnessKey userstate -> (MutableRead m (EditReader edit) -> [edit] -> StateT userstate m ()) -> StateT userstate m ()) -> StateT (UpdateStore edit) m ();
    runUpdateStore call = traverseWitnessStoreStateT $ \key -> (runUpdateStoreEntry $ call key) >> return ();

    updateStore :: IsStateIO m => MutableRead m (EditReader edit) -> [edit] -> StateT (UpdateStore edit) m ();
    updateStore mutr edits = runUpdateStore $ \_ ff -> ff mutr edits;

    makeSharedSubscriber :: forall edit actions. Subscriber edit actions -> IO (SubscriberW edit actions);
    makeSharedSubscriber parent = do
    {
        let
        {
            firstP :: UpdateStore edit;
            firstP = emptyWitnessStore;

            initP :: Object edit (UpdateStore edit) -> IO (Object edit (UpdateStore edit));
            initP objectP = return objectP;

            updateP :: forall m. IsStateIO m => Object edit (UpdateStore edit) -> MutableRead m (EditReader edit) -> [edit] -> StateT (UpdateStore edit) m ();
            updateP _ mutrP edits = updateStore mutrP edits;
        };
        (MkObject objectP,closerP,actions) <- parent firstP initP updateP;
        let
        {
            objectC :: forall userstate. IOWitnessKey userstate -> Object edit userstate;
            objectC key = MkObject $ \call -> objectP $ \(mutedP :: MutableEdit m edit) (accP :: StateAccess m (UpdateStore edit)) -> let
            {
                accC :: StateAccess m userstate;
                accC stu = accP $ replaceOneWitnessStoreStateT key $ lensStateT useStateLens stu;
            } in call mutedP accC;

            child :: Subscriber edit actions;
            child firstC initC updateC = do
            {
                rec
                {
                    key <- objectP $ \_ accP -> accP $ remonad liftIO $ addIOWitnessStoreStateT (MkStoreEntry (updateC editorC) firstC);
                    editorC <- initC (objectC key);
                };
                let
                {
                    closerC = objectP $ \_ accP -> accP $ do
                    {
                        deleteWitnessStoreStateT key;
                        newstore <- get;
                        if isEmptyWitnessStore newstore
                         then liftIO closerP;
                         else return ();
                    };
                };
                return (editorC,closerC,actions);
            };
        };
        return $ MkSubscriberW child;
    };

    objectSubscriber :: Object edit () -> SubscriberW edit ();
    objectSubscriber (MkObject object) = MkSubscriberW $ \firstState initr update -> do
    {
        var <- newMVar firstState;
        rec
        {
            editor <- initr $ MkObject $ \call -> object $ \muted _ -> let
            {
                muted' = MkMutableEdit
                {
                    mutableRead = mutableRead muted,
                    mutableEdit = \edits -> do
                    {
                        maction <- mutableEdit muted edits;
                        case maction of
                        {
                            Nothing -> return Nothing;
                            Just action -> return $ Just $ do
                            {
                                action;
                                mvarStateAccess var $ update editor (mutableRead muted) edits;
                            }
                        }
                    }
                };
            } in call muted' $ mvarStateAccess var;
        };
        return (editor,return (),());
    };

    makeObjectSubscriber :: Object edit () -> IO (SubscriberW edit ());
    makeObjectSubscriber object = let
    {
        MkSubscriberW sub = objectSubscriber object;
    } in makeSharedSubscriber sub;
}
