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


    type Subscriber edit actions = forall editor.
        (Object edit -> IO editor) -> -- initialise: provides read MutableEdit, initial allowed, write MutableEdit
        (forall m. IsStateIO m => editor -> MutableRead m (EditReader edit) -> [edit] -> m ()) -> -- receive: get updates (both others and from your mutableEdit calls)
        IO (editor,IO (),actions);

    newtype SubscriberW edit a = MkSubscriberW (Subscriber edit a);

    instance Functor (SubscriberW edit) where
    {
        fmap ab (MkSubscriberW sub) = MkSubscriberW $ \initialise receive -> do
        {
            (editor,cl,a) <- sub initialise receive;
            return (editor,cl,ab a);
        };
    };

    newtype UpdateStoreEntry edit = MkStoreEntry (forall m. IsStateIO m => MutableRead m (EditReader edit) -> [edit] -> m ());

    type UpdateStore edit = Store (UpdateStoreEntry edit);

    runUpdateStoreEntry :: IsStateIO m => ((MutableRead m (EditReader edit) -> [edit] -> m ()) -> m ()) -> StateT (UpdateStoreEntry edit) m ();
    runUpdateStoreEntry call = do
    {
        MkStoreEntry update <- get;
        lift $ call update;
    };

    runUpdateStore :: IsStateIO m => (Key -> (MutableRead m (EditReader edit) -> [edit] -> m ()) -> m ()) -> StateT (UpdateStore edit) m ();
    runUpdateStore call = traverseStoreStateT $ \key -> (runUpdateStoreEntry $ call key) >> return ();

    updateStore :: IsStateIO m => MutableRead m (EditReader edit) -> [edit] -> StateT (UpdateStore edit) m ();
    updateStore mutr edits = runUpdateStore $ \_ ff -> ff mutr edits;

    makeSharedSubscriber :: forall edit actions. Subscriber edit actions -> IO (SubscriberW edit actions);
    makeSharedSubscriber parent = do
    {
        var <- newMVar emptyStore;
        let
        {
            initP :: Object edit -> IO (Object edit);
            initP objectP = return objectP;

            updateP :: forall m. IsStateIO m => Object edit -> MutableRead m (EditReader edit) -> [edit] -> m ();
            updateP _ mutrP edits = mvarStateAccess var $ updateStore mutrP edits;
        };
        (MkObject objectP,closerP,actions) <- parent initP updateP;
        let
        {
            objectC :: Object edit;
            objectC = MkObject objectP;

            child :: Subscriber edit actions;
            child initC updateC = do
            {
                editorC <- initC objectC;
                key <- objectP $ \_ -> mvarStateAccess var $ remonad liftIO $ addStoreStateT $ MkStoreEntry $ updateC editorC;
                let
                {
                    closerC = objectP $ \_ -> mvarStateAccess var $ do
                    {
                        deleteStoreStateT key;
                        newstore <- get;
                        if isEmptyStore newstore
                         then liftIO closerP;
                         else return ();
                    };
                };
                return (editorC,closerC,actions);
            };
        };
        return $ MkSubscriberW child;
    };

    objectSubscriber :: Object edit -> SubscriberW edit ();
    objectSubscriber (MkObject object) = MkSubscriberW $ \initr update -> do
    {
        rec
        {
            editor <- initr $ MkObject $ \call -> object $ \muted -> let
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
                                update editor (mutableRead muted) edits;
                            }
                        }
                    }
                };
            } in call muted';
        };
        return (editor,return (),());
    };

    makeObjectSubscriber :: Object edit -> IO (SubscriberW edit ());
    makeObjectSubscriber object = let
    {
        MkSubscriberW sub = objectSubscriber object;
    } in makeSharedSubscriber sub;
}
