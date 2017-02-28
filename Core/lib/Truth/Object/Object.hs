module Truth.Object.Object where
{
    import Truth.Edit.Import;
    import Data.IORef;
    import Truth.Edit;


    data API m edit token = MkAPI
    {
        apiRead :: Structure m (EditReader edit),
        apiAllowed :: edit -> m Bool,
        apiEdit :: edit -> m token
    };

    instance Functor m => Functor (API m edit) where
    {
        fmap ab (MkAPI r a e) = MkAPI r a $ fmap (fmap ab) e;
    };

    type LockAPI edit token = forall r. (token -> API IO edit token -> IO r) -> IO r;

    mapLockAPIToken :: (token1 -> token2) -> LockAPI edit token1 -> LockAPI edit token2;
    mapLockAPIToken t1t2 lapi1 ff = lapi1 $ \token api -> ff (t1t2 token) (fmap t1t2 api);

    nonlockingAPI :: token -> API IO edit token -> LockAPI edit token;
    nonlockingAPI token api ff =  ff token api;

    newtype LockAPIW edit token = MkLockAPIW (LockAPI edit token);

    freeLockAPI :: forall edit. (Edit edit,FullReader (EditReader edit)) => EditSubject edit -> (EditSubject edit -> Bool) -> IO (LockAPIW edit ());
    freeLockAPI firsta allowed = do
    {
        var <- newMVar firsta;
        let
        {
            lapi :: LockAPI edit ();
            lapi ff = modifyMVar var $ \olda -> do
            {
                ref <- newIORef olda;
                let
                {
                    api = MkAPI
                    {
                        apiRead = readFromM $ readIORef ref,
                        apiAllowed = \edit -> do
                        {
                            oa <- readIORef ref;
                            let
                            {
                                na = fromReadFunction (applyEdit edit) oa;
                            };
                            return $ allowed na;
                        },
                        apiEdit = \edit -> do
                        {
                            oa <- readIORef ref;
                            let
                            {
                                na = fromReadFunction (applyEdit edit) oa;
                            };
                            writeIORef ref na;
                            return ();
                        }
                    };
                };
                r <- ff () api;
                newa <- readIORef ref;
                return (newa,r);
            };
        };
        return $ MkLockAPIW lapi;
    };


    type Object edit = forall editor token.
        (LockAPI edit token -> IO (editor,token)) -> -- initialise: provides read API, initial allowed, write API
        (editor -> token -> edit -> IO token) -> -- receive: get updates (both others and from your apiEdit calls)
        IO (editor, IO ());

    newtype ObjectW edit = MkObjectW (Object edit);

    data StoreEntry edit token = MkStoreEntry (token -> edit -> IO token) token;

    shareObject :: forall edit. Object edit -> IO (ObjectW edit);
    shareObject parent = do
    {
        storevar <- newMVar (emptyWitnessStore :: IOWitnessStore (StoreEntry edit));
        let
        {
            initP :: LockAPI edit () -> IO (LockAPIW edit (),());
            initP lapiP = do
            {
                let
                {
                    tokenP = ();
                };
                return (MkLockAPIW lapiP,tokenP);
            };

            updateP :: LockAPIW edit () -> () -> edit -> IO ();
            updateP (MkLockAPIW _lapiP) oldtokenP edit = modifyMVar storevar $ \oldstore -> do
            {
                newstore <- traverseWitnessStore (\_ (MkStoreEntry u oldtoken) -> do
                {
                    newtoken <- u oldtoken edit;
                    return (MkStoreEntry u newtoken);
                }) oldstore;
                let
                {
                    newtokenP = oldtokenP;
                };
                return (newstore,newtokenP);
            };
        };
        (MkLockAPIW lapiP,closerP) <- parent initP updateP;
        let
        {
            lapiC :: forall token. IOWitnessKey token -> LockAPI edit token;
            lapiC key ff = modifyMVar storevar $ \store -> lapiP $ \_tokenP (apiP :: API IO edit ()) -> do
            {
                storeRef <- newIORef store;
                let
                {
                    apiC :: API IO edit token;
                    apiC = MkAPI
                    {
                        apiRead = \rt -> apiRead apiP rt,
                        apiAllowed = \edit -> apiAllowed apiP edit,
                        apiEdit = \edit -> do
                        {
                            _nextTokenP <- apiEdit apiP edit;
                            tokenRef <- newIORef Nothing;
                            oldstore <- readIORef storeRef;
                            newstore <- traverseWitnessStore (\keyf (MkStoreEntry u oldtoken) -> do
                            {
                                newtoken <- u oldtoken edit;
                                case testEquality key keyf of
                                {
                                    Just Refl -> writeIORef tokenRef $ Just newtoken;
                                    Nothing -> return ();
                                };
                                return (MkStoreEntry u newtoken);
                            }) oldstore;
                            writeIORef storeRef newstore;
                            mnextTokenC <- readIORef tokenRef;
                            return $ fromJust mnextTokenC;
                        }
                    };

                    tokenC :: token;
                    (MkStoreEntry _ tokenC) = fromJust $ lookupWitnessStore key store;
                };
                r <- ff tokenC apiC;
                newstore <- readIORef storeRef;
                return (newstore,r);
            };

            child :: Object edit;
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
        return $ MkObjectW child;
    };

    shareLockAPI :: LockAPI edit token -> IO (ObjectW edit);
    shareLockAPI lapi = shareObject $ \initr _update -> do
    {
        rec
        {
            (editor,token) <- initr $ mapLockAPIToken (\_ -> token) lapi;
        };
        return (editor,return ());
    }
}
