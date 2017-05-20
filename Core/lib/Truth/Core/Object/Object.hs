module Truth.Core.Object.Object
(
    module Truth.Core.Object.Object,
    liftIO,
) where
{
    import Truth.Core.Import;
    import Data.IORef;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Object.MutableEdit;
    import Truth.Core.Object.LockAPI;


    type Object edit = forall editor userstate.
        (LockAPI edit userstate -> IO (editor,userstate)) -> -- initialise: provides read MutableEdit, initial allowed, write MutableEdit
        (forall m. MonadIOInvert m => editor -> MutableRead m (EditReader edit) -> userstate -> [edit] -> m userstate) -> -- receive: get updates (both others and from your mutableEdit calls)
        IO (editor, IO ());

    newtype ObjectW edit = MkObjectW (Object edit);

    data StoreEntry edit userstate = MkStoreEntry (forall m. MonadIOInvert m => MutableRead m (EditReader edit) -> userstate -> [edit] -> m userstate) userstate;

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

            updateP :: forall m. MonadIOInvert m => LockAPI edit () -> MutableRead m (EditReader edit) -> () -> [edit] -> m ();
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
        (MkLockAPI lapiP,closerP) <- parent initP updateP;
        let
        {
            lapiC :: forall userstate. IOWitnessKey userstate -> LockAPI edit userstate;
            lapiC key = MkLockAPI $ \ff -> modifyMVar storevar $ \store -> lapiP $ \_tokenP (apiP :: MutableEdit m edit ()) -> do
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
