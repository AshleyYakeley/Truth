module Truth.Core.Object.Object
(
    module Truth.Core.Object.Object,
    liftIO,
) where
{
    import Truth.Core.Import;
    import Data.IORef;
    import Truth.Core.Object.API;


    type Object edit = forall editor userstate.
        (LockAPI edit userstate -> IO (editor,userstate)) -> -- initialise: provides read API, initial allowed, write API
        (editor -> userstate -> [edit] -> IO userstate) -> -- receive: get updates (both others and from your apiEdit calls)
        IO (editor, IO ());

    newtype ObjectW edit = MkObjectW (Object edit);

    data StoreEntry edit userstate = MkStoreEntry (userstate -> [edit] -> IO userstate) userstate;

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
            lapiC :: forall userstate. IOWitnessKey userstate -> LockAPI edit userstate;
            lapiC key = MkLockAPI $ \ff -> modifyMVar storevar $ \store -> lapiP $ \_tokenP (apiP :: API m edit ()) -> do
            {
                let
                {
                    apiC :: API (StateT (IOWitnessStore (StoreEntry edit)) m) edit userstate;
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

                    tokenC :: userstate;
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

    shareLockAPI :: LockAPI edit userstate -> IO (ObjectW edit);
    shareLockAPI lapi = shareObject $ \initr _update -> do
    {
        rec
        {
            (editor,userstate) <- initr $ fmap (\_ -> userstate) lapi;
        };
        return (editor,return ());
    }
}
