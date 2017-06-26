module Truth.Core.Object.Subscription
(
    Subscription,SubscriptionW(..),subscribeObject,
    liftIO,
) where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Object.MutableEdit;
    import Truth.Core.Object.Object;


    type Subscription edit a = forall editor userstate.
        userstate ->
        (Object edit userstate -> IO (editor,userstate)) -> -- initialise: provides read MutableEdit, initial allowed, write MutableEdit
        (forall m. IsStateIO m => editor -> MutableRead m (EditReader edit) -> [edit] -> StateT userstate m ()) -> -- receive: get updates (both others and from your mutableEdit calls)
        IO (editor, a);

    newtype SubscriptionW edit a = MkSubscriptionW (Subscription edit a);

    instance Functor (SubscriptionW edit) where
    {
        fmap ab (MkSubscriptionW sub) = MkSubscriptionW $ \fs initialise receive -> do
        {
            (editor,a) <- sub fs initialise receive;
            return (editor,ab a);
        };
    };

    data UpdateStoreEntry edit userstate = MkStoreEntry (forall m. IsStateIO m => MutableRead m (EditReader edit) -> [edit] -> StateT userstate m ()) userstate;
    type UpdateStore edit = IOWitnessStore (UpdateStoreEntry edit);

    runUpdateStoreEntry :: IsStateIO m => ((MutableRead m (EditReader edit) -> [edit] -> StateT userstate m ()) -> StateT userstate m ()) -> UpdateStoreEntry edit userstate -> m (UpdateStoreEntry edit userstate);
    runUpdateStoreEntry call (MkStoreEntry update oldstate) = do
    {
        ((),newstate) <- runStateT (call update) oldstate;
        return $ MkStoreEntry update newstate;
    };

    runUpdateStore :: IsStateIO m => (forall userstate. IOWitnessKey userstate -> (MutableRead m (EditReader edit) -> [edit] -> StateT userstate m ()) -> StateT userstate m ()) -> UpdateStore edit -> m (UpdateStore edit);
    runUpdateStore call = traverseWitnessStore $ \key -> runUpdateStoreEntry $ call key;

    updateStore :: IsStateIO m => MutableRead m (EditReader edit) -> [edit] -> UpdateStore edit -> m (UpdateStore edit);
    updateStore mutr edits = runUpdateStore $ \_ ff -> ff mutr edits;

    shareSubscription :: forall edit. Subscription edit (IO ()) -> IO (SubscriptionW edit (IO ()));
    shareSubscription parent = do
    {
        storevar <- newMVar (emptyWitnessStore :: UpdateStore edit);
        let
        {
            firstP :: ();
            firstP = ();

            initP :: Object edit () -> IO (Object edit (),());
            initP objectP = do
            {
                let
                {
                    tokenP = ();
                };
                return (objectP,tokenP);
            };

            updateP :: forall m. IsStateIO m => Object edit () -> MutableRead m (EditReader edit) -> [edit] -> StateT () m ();
            updateP _ mutrP edits = mapIOInvert (modifyMVar storevar) $ \oldstore -> do
            {
                newstore <- lift $ updateStore mutrP edits oldstore;
                return (newstore,());
            };
        };
        (MkObject objectP,closerP) <- parent firstP initP updateP;
        let
        {
            objectC :: forall userstate. IOWitnessKey userstate -> Object edit userstate;
            objectC key = MkObject $ \call -> modifyMVar storevar $ \oldstore -> objectP $ \(mutedP :: MutableEdit m edit ()) -> do
            {
                let
                {
                    mutedC :: MutableEdit (StateT (UpdateStore edit) m) edit userstate;
                    mutedC = MkMutableEdit
                    {
                        mutableRead = \rt -> lift $ mutableRead mutedP rt,
                        mutableEdit = \edits -> do
                        {
                            mmnextTokenP <- lift $ mutableEdit mutedP edits;
                            case mmnextTokenP of
                            {
                                Just mnextTokenP -> do
                                {
                                    lnextTokenC <- traverseWitnessStoreStateT (\keyf -> do
                                    {
                                        MkStoreEntry u oldtoken <- get;
                                        ((),newtoken) <- lift $ runStateT (u (mutableRead mutedP) edits) oldtoken;
                                        put $ MkStoreEntry u newtoken;
                                        case testEquality key keyf of
                                        {
                                            Just Refl -> return [newtoken];
                                            Nothing -> return [];
                                        };
                                    });
                                    return $ case lnextTokenC of
                                    {
                                        [nextTokenC] -> Just $ do
                                        {
                                            _nextTokenP <- lift mnextTokenP;
                                            return nextTokenC;
                                        };
                                        _ -> Nothing;
                                    };
                                };
                                Nothing -> return Nothing;
                            }
                        }
                    };

                    tokenC :: userstate;
                    (MkStoreEntry _ tokenC) = fromJust $ lookupWitnessStore key oldstore;
                };
                ((r,_newTokenC),newstore) <- unitStateT $ runStateT (runStateT (call mutedC) tokenC) oldstore;
                return (newstore,r);
            };

            child :: Subscription edit (IO ());
            child firstC initC updateC = do
            {
                rec
                {
                    key <- modifyMVar storevar $ \oldstore -> do
                    {
                        (k,newstore) <- addIOWitnessStore (MkStoreEntry (updateC editorC) firstC) oldstore;
                        return (newstore,k);
                    };
                    (editorC,tokenC) <- initC (objectC key);
                };
                modifyMVar_ storevar $ \oldstore -> return $ replaceWitnessStore key (\(MkStoreEntry u _) -> MkStoreEntry u tokenC) oldstore;
                let
                {
                    closerC = modifyMVar_ storevar $ \oldstore -> do
                    {
                        let
                        {
                            newstore = deleteWitnessStore key oldstore;
                        };
                        if isEmptyWitnessStore newstore
                         then closerP;
                         else return ();
                        return newstore;
                    };
                };
                return (editorC,closerC);
            };
        };
        return $ MkSubscriptionW child;
    };

    rawSubscribeObject :: Object edit () -> SubscriptionW edit ();
    rawSubscribeObject object = MkSubscriptionW $ \firstState initr _update -> do
    {
        let
        {
            lensGet _ = firstState;
            lensPutback _ _ = Identity ();
            dubiousLens = MkLens{..};
        };
        (editor,_userstate) <- initr $ lensObject dubiousLens object;
        return (editor,());
    };

    subscribeObject :: Object edit () -> IO (SubscriptionW edit (IO ()));
    subscribeObject object = let
    {
        MkSubscriptionW sub = fmap return $ rawSubscribeObject object;
    } in shareSubscription sub;
}
