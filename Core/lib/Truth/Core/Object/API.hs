module Truth.Core.Object.API where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;


    data API m edit userstate = MkAPI
    {
        apiRead :: Structure m (EditReader edit),
        apiAllowed :: edit -> m Bool,
        apiEdit :: [edit] -> m (Maybe userstate)
    };

    apiReadEdit :: Monad m => API m edit userstate -> Readable (EditReader edit) [edit] -> m (Maybe userstate);
    apiReadEdit MkAPI{..}  MkReadable{..} = do
    {
        edits <- unReadable apiRead;
        apiEdit edits;
    };

    apiAlloweds :: Monad m => API m edit userstate -> [edit] -> m Bool;
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

    newtype LockAPI edit userstate = MkLockAPI (forall r. (forall m. MonadIOInvert m => userstate -> API m edit userstate -> m r) -> IO r);

    instance Functor (LockAPI edit) where
    {
        fmap t1t2 (MkLockAPI lapi1) = MkLockAPI $ \ff -> lapi1 $ \userstate api -> ff (t1t2 userstate) (fmap t1t2 api);
    };

    nonlockingAPI :: userstate -> API IO edit userstate -> LockAPI edit userstate;
    nonlockingAPI userstate api = MkLockAPI $ \ff -> ff userstate api;

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

    mapLockAPI :: forall f lensstate edita editb userstate. MonadOne f => FloatingEditLens' f lensstate edita editb -> LockAPI edita (userstate,lensstate) -> LockAPI editb userstate;
    mapLockAPI lens@MkFloatingEditLens{..} (MkLockAPI lapiA) = MkLockAPI $ \(callB :: forall m. MonadIOInvert m => userstate -> API m editb userstate -> m r) -> let
    {
        MkFloatingEditFunction{..} = floatingEditLensFunction;

        callA :: forall m. MonadIOInvert m => (userstate,lensstate) -> API m edita (userstate,lensstate) -> m r;
        callA (firstuserstate,firstlensstate) (apiA :: API m edita (userstate,lensstate)) = let
        {
            readA :: Structure m (EditReader edita);
            _allowedA :: edita -> m Bool;
            pushEditA :: [edita] -> m (Maybe (userstate,lensstate));
            MkAPI readA _allowedA pushEditA = apiA;

            readB :: Structure (StateT lensstate m) (EditReader editb);
            readB rt = do
            {
                state <- get;
                lift $ mapStructure (floatingEditGet state) readA rt;
            };

            convertEdit :: [editb] -> (StateT lensstate m) (Maybe [edita]);
            convertEdit editBs = do
            {
                oldstate <- get;
                fstateeditA :: f (state,[edita]) <- lift $ unReadable (floatingEditLensPutEdits lens oldstate editBs) readA;
                case getMaybeOne fstateeditA of
                {
                    Just (newstate,editAs) -> do
                    {
                        put newstate;
                        return $ Just editAs;
                    };
                    Nothing -> return Nothing;
                };
            };

            allowedB :: editb -> (StateT lensstate m) Bool;
            allowedB editB = do
            {
                state <- get;
                lensOK <- lift $ unReadable (floatingEditLensAllowed lens state editB) readA;
                case lensOK of
                {
                    False -> return False;
                    True -> do
                    {
                        meditA <- convertEdit [editB];
                        case meditA of
                        {
                            Nothing -> return False; -- is this correct?
                            Just editA -> lift $ apiAlloweds apiA editA;
                        };
                    };
                };
            };

            pushEditB :: [editb] -> (StateT lensstate m) (Maybe userstate);
            pushEditB editB = do
            {
                meditA <- convertEdit editB;
                case meditA of
                {
                    Nothing -> return $ Just firstuserstate; -- is this correct?
                    Just editAs -> do
                    {
                        mstates <- lift $ pushEditA editAs;
                        return $ fmap fst mstates;
                    };
                };
            };

            apiB :: API (StateT lensstate m) editb userstate;
            apiB = MkAPI readB allowedB pushEditB;
        }
        in liftIOInvert $ \unlift -> do
        {
            (o,(r,_newstate)) <- unlift $ runStateT (callB firstuserstate apiB) firstlensstate; -- just throw away the new lens state: all these lens changes will be replayed by the update
            return (o,r);
        };
    } in lapiA callA;
}
