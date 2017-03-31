module Truth.Object.Object
(
    module Truth.Object.Object,
    liftIO,
) where
{
    import Truth.Edit.Import;
    import Data.IORef;
    import Truth.Edit;


    data API m edit token = MkAPI
    {
        apiRead :: Structure m (EditReader edit),
        apiAllowed :: edit -> m Bool,
        apiEdit :: [edit] -> m (Maybe token)
    };

    apiReadEdit :: Monad m => API m edit token -> Readable (EditReader edit) [edit] -> m (Maybe token);
    apiReadEdit MkAPI{..}  MkReadable{..} = do
    {
        edits <- unReadable apiRead;
        apiEdit edits;
    };

    apiAlloweds :: Monad m => API m edit token -> [edit] -> m Bool;
    apiAlloweds _api [] = return True;
    apiAlloweds api (e:ee) = do
    {
        allowed <- apiAllowed api e;
        if allowed then apiAlloweds api ee else return False;
    };

    singleApiEdit :: Applicative m => (edit -> m (Maybe ())) -> [edit] -> m (Maybe ());
    singleApiEdit apiEdit' edits = fmap combine $ traverse apiEdit' edits where
    {
        combine :: [Maybe ()] -> Maybe ();
        combine [] = return ();
        combine (m:mm) = do
        {
            () <- m;
            combine mm;
        }
    };

    instance Functor m => Functor (API m edit) where
    {
        fmap ab (MkAPI r a e) = MkAPI r a $ fmap (fmap (fmap ab)) e;
    };

    newtype LockAPI edit token = MkLockAPI (forall r. (forall m. MonadIOInvert m => token -> API m edit token -> m r) -> IO r);

    instance Functor (LockAPI edit) where
    {
        fmap t1t2 (MkLockAPI lapi1) = MkLockAPI $ \ff -> lapi1 $ \token api -> ff (t1t2 token) (fmap t1t2 api);
    };

    nonlockingAPI :: token -> API IO edit token -> LockAPI edit token;
    nonlockingAPI token api = MkLockAPI $ \ff -> ff token api;

    freeLockAPI :: forall edit. (Edit edit,FullReader (EditReader edit)) => EditSubject edit -> (EditSubject edit -> Bool) -> IO (LockAPI edit ());
    freeLockAPI firsta allowed = do
    {
        var <- newMVar firsta;
        let
        {
            lapi :: LockAPI edit ();
            lapi = MkLockAPI $ \ff -> modifyMVar var $ \olda -> do
            {
                let
                {
                    api :: API (StateT (EditSubject edit) IO) edit ();
                    api = MkAPI
                    {
                        apiRead = readFromM $ get,
                        apiAllowed = \edit -> do
                        {
                            oa <- get;
                            let
                            {
                                na = fromReadFunction (applyEdit edit) oa;
                            };
                            return $ allowed na;
                        },
                        apiEdit = \edits -> do
                        {
                            oa <- get;
                            let
                            {
                                na = fromReadFunction (applyEdits edits) oa;
                            };
                            if allowed na then do
                            {
                                put na;
                                return $ Just ();
                            }
                            else return Nothing;
                        }
                    };
                };
                (r,newa) <- runStateT (ff () api) olda;
                return (newa,r);
            };
        };
        return lapi;
    };


    type Object edit = forall editor token.
        (LockAPI edit token -> IO (editor,token)) -> -- initialise: provides read API, initial allowed, write API
        (editor -> token -> [edit] -> IO token) -> -- receive: get updates (both others and from your apiEdit calls)
        IO (editor, IO ());

    newtype ObjectW edit = MkObjectW (Object edit);

    data StoreEntry edit token = MkStoreEntry (token -> [edit] -> IO token) token;

    shareObject :: forall edit. Object edit -> IO (ObjectW edit);
    shareObject parent = do
    {
        storevar <- newMVar (emptyWitnessStore :: IOWitnessStore (StoreEntry edit));
        let
        {
            initP :: LockAPI edit () -> IO (LockAPI edit (),());
            initP lapiP = do
            {
                let
                {
                    tokenP = ();
                };
                return (lapiP,tokenP);
            };

            updateP :: LockAPI edit () -> () -> [edit] -> IO ();
            updateP (MkLockAPI _lapiP) oldtokenP edits = modifyMVar storevar $ \oldstore -> do
            {
                newstore <- traverseWitnessStore (\_ (MkStoreEntry u oldtoken) -> do
                {
                    newtoken <- u oldtoken edits;
                    return (MkStoreEntry u newtoken);
                }) oldstore;
                let
                {
                    newtokenP = oldtokenP;
                };
                return (newstore,newtokenP);
            };
        };
        (MkLockAPI lapiP,closerP) <- parent initP updateP;
        let
        {
            lapiC :: forall token. IOWitnessKey token -> LockAPI edit token;
            lapiC key = MkLockAPI $ \ff -> modifyMVar storevar $ \store -> lapiP $ \_tokenP (apiP :: API m edit ()) -> do
            {
                let
                {
                    apiC :: API (StateT (IOWitnessStore (StoreEntry edit)) m) edit token;
                    apiC = MkAPI
                    {
                        apiRead = \rt -> lift $ apiRead apiP rt,
                        apiAllowed = \edit -> lift $ apiAllowed apiP edit,
                        apiEdit = \edits -> do
                        {
                            mnextTokenP <- lift $ apiEdit apiP edits;
                            case mnextTokenP of
                            {
                                Just _nextTokenP -> do
                                {
                                    tokenRef <- liftIO $ newIORef Nothing;
                                    oldstore <- get;
                                    newstore <- traverseWitnessStore (\keyf (MkStoreEntry u oldtoken) -> do
                                    {
                                        newtoken <- liftIO $ u oldtoken edits;
                                        case testEquality key keyf of
                                        {
                                            Just Refl -> liftIO $ writeIORef tokenRef $ Just newtoken;
                                            Nothing -> return ();
                                        };
                                        return (MkStoreEntry u newtoken);
                                    }) oldstore;
                                    put newstore;
                                    mnextTokenC <- liftIO $ readIORef tokenRef;
                                    return $ mnextTokenC;
                                };
                                Nothing -> return Nothing;
                            }
                        }
                    };

                    tokenC :: token;
                    (MkStoreEntry _ tokenC) = fromJust $ lookupWitnessStore key store;
                };
                (r,newstore) <- runStateT (ff tokenC apiC) store;
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
            (editor,token) <- initr $ fmap (\_ -> token) lapi;
        };
        return (editor,return ());
    }
}
