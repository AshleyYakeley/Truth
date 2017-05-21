module Truth.Core.Object.Subscription
(
    module Truth.Core.Object.Subscription,
    liftIO,
) where
{
    import Truth.Core.Import;
    import Data.IORef;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Object.MutableEdit;
    import Truth.Core.Object.Object;


    type Subscription edit = forall editor userstate.
        (Object edit userstate -> IO (editor,userstate)) -> -- initialise: provides read MutableEdit, initial allowed, write MutableEdit
        (forall m. MonadIOInvert m => editor -> MutableRead m (EditReader edit) -> userstate -> [edit] -> m userstate) -> -- receive: get updates (both others and from your mutableEdit calls)
        IO (editor, IO ());

    newtype SubscriptionW edit = MkSubscriptionW (Subscription edit);

    data StoreEntry edit userstate = MkStoreEntry (forall m. MonadIOInvert m => MutableRead m (EditReader edit) -> userstate -> [edit] -> m userstate) userstate;

    shareSubscription :: forall edit. Subscription edit -> IO (SubscriptionW edit);
    shareSubscription parent = do
    {
        storevar <- newMVar (emptyWitnessStore :: IOWitnessStore (StoreEntry edit));
        let
        {
            initP :: Object edit () -> IO (Object edit (),());
            initP lapiP = do
            {
                let
                {
                    tokenP = ();
                };
                return (lapiP,tokenP);
            };

            updateP :: forall m. MonadIOInvert m => Object edit () -> MutableRead m (EditReader edit) -> () -> [edit] -> m ();
            updateP _ meP oldtokenP edits = mapIOInvert (modifyMVar storevar) $ \oldstore -> do
            {
                newstore <- traverseWitnessStore (\_ (MkStoreEntry u oldtoken) -> do
                {
                    newtoken <- u meP oldtoken edits;
                    return (MkStoreEntry u newtoken);
                }) oldstore;
                let
                {
                    newtokenP = oldtokenP;
                };
                return (newstore,newtokenP);
            };
        };
        (MkObject lapiP,closerP) <- parent initP updateP;
        let
        {
            lapiC :: forall userstate. IOWitnessKey userstate -> Object edit userstate;
            lapiC key = MkObject $ \ff -> modifyMVar storevar $ \store -> lapiP $ \_tokenP (apiP :: MutableEdit m edit ()) -> do
            {
                let
                {
                    apiC :: MutableEdit (StateT (IOWitnessStore (StoreEntry edit)) m) edit userstate;
                    apiC = MkMutableEdit
                    {
                        mutableRead = \rt -> lift $ mutableRead apiP rt,
                        mutableEdit = \edits -> do
                        {
                            mmnextTokenP <- lift $ mutableEdit apiP edits;
                            case mmnextTokenP of
                            {
                                Just mnextTokenP -> do
                                {
                                    tokenRef <- liftIO $ newIORef Nothing;
                                    oldstore <- get;
                                    newstore <- traverseWitnessStore (\keyf (MkStoreEntry u oldtoken) -> do
                                    {
                                        newtoken <- lift $ u (mutableRead apiP) oldtoken edits;
                                        case testEquality key keyf of
                                        {
                                            Just Refl -> liftIO $ writeIORef tokenRef $ Just newtoken;
                                            Nothing -> return ();
                                        };
                                        return (MkStoreEntry u newtoken);
                                    }) oldstore;
                                    put newstore;
                                    mnextTokenC <- liftIO $ readIORef tokenRef;
                                    return $ case mnextTokenC of
                                    {
                                        Just nextTokenC -> Just $ do
                                        {
                                            _nextTokenP <- lift mnextTokenP;
                                            return nextTokenC;
                                        };
                                        Nothing -> Nothing;
                                    };
                                };
                                Nothing -> return Nothing;
                            }
                        }
                    };

                    tokenC :: userstate;
                    (MkStoreEntry _ tokenC) = fromJust $ lookupWitnessStore key store;
                };
                (r,newstore) <- runStateT (ff tokenC apiC) store;
                return (newstore,r);
            };

            child :: Subscription edit;
            child initC updateC = do
            {
                rec
                {
                    (editorC,tokenC) <- initC (lapiC key);
                    key <- modifyMVar storevar $ \oldstore -> do
                    {
                        (k,newstore) <- addIOWitnessStore (MkStoreEntry (updateC editorC) tokenC) oldstore;
                        return (newstore,k);
                    };
                };
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

    subscribeObject :: Object edit userstate -> IO (SubscriptionW edit);
    subscribeObject lapi = shareSubscription $ \initr _update -> do
    {
        rec
        {
            (editor,userstate) <- initr $ fmap (\_ -> userstate) lapi;
        };
        return (editor,return ());
    }
}
