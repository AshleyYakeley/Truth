module Truth.Core.Object.Subscription
(
    Subscription,SubscriptionW(..),subscribeObject,
    liftIO,
) where
{
    import Truth.Core.Import;
    import Data.IORef;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Object.MutableEdit;
    import Truth.Core.Object.Object;


    type Subscription edit a = forall editor userstate.
        userstate ->
        (Object edit userstate -> IO (editor,userstate)) -> -- initialise: provides read MutableEdit, initial allowed, write MutableEdit
        (forall m. MonadIOInvert m => editor -> MutableRead m (EditReader edit) -> userstate -> [edit] -> m userstate) -> -- receive: get updates (both others and from your mutableEdit calls)
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

    data StoreEntry edit userstate = MkStoreEntry (forall m. MonadIOInvert m => MutableRead m (EditReader edit) -> userstate -> [edit] -> m userstate) userstate;

    shareSubscription :: forall edit. Subscription edit (IO ()) -> IO (SubscriptionW edit (IO ()));
    shareSubscription parent = do
    {
        storevar <- newMVar (emptyWitnessStore :: IOWitnessStore (StoreEntry edit));
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

            updateP :: forall m. MonadIOInvert m => Object edit () -> MutableRead m (EditReader edit) -> () -> [edit] -> m ();
            updateP _ mutrP oldtokenP edits = mapIOInvert (modifyMVar storevar) $ \oldstore -> do
            {
                newstore <- traverseWitnessStore (\_ (MkStoreEntry u oldtoken) -> do
                {
                    newtoken <- u mutrP oldtoken edits;
                    return (MkStoreEntry u newtoken);
                }) oldstore;
                let
                {
                    newtokenP = oldtokenP;
                };
                return (newstore,newtokenP);
            };
        };
        (MkObject objectP,closerP) <- parent firstP initP updateP;
        let
        {
            objectC :: forall userstate. IOWitnessKey userstate -> Object edit userstate;
            objectC key = MkObject $ \ff -> modifyMVar storevar $ \store -> objectP $ \_tokenP (mutedP :: MutableEdit m edit ()) -> do
            {
                let
                {
                    mutedC :: MutableEdit (StateT (IOWitnessStore (StoreEntry edit)) m) edit userstate;
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
                                    tokenRef <- liftIO $ newIORef Nothing;
                                    oldstore <- get;
                                    newstore <- traverseWitnessStore (\keyf (MkStoreEntry u oldtoken) -> do
                                    {
                                        newtoken <- lift $ u (mutableRead mutedP) oldtoken edits;
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
                (r,newstore) <- runStateT (ff tokenC mutedC) store;
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
        (editor,_userstate) <- initr $ fmap (\_ -> firstState) object;
        return (editor,());
    };

    subscribeObject :: Object edit () -> IO (SubscriptionW edit (IO ()));
    subscribeObject object = let
    {
        MkSubscriptionW sub = fmap return $ rawSubscribeObject object;
    } in shareSubscription sub;
}
