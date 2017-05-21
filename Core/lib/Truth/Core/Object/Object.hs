module Truth.Core.Object.Object where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Pair;
    import Truth.Core.Object.MutableEdit;


    newtype Object edit userstate = MkObject (forall r. (forall m. MonadIOInvert m => userstate -> MutableEdit m edit userstate -> m r) -> IO r);

    instance Functor (Object edit) where
    {
        fmap t1t2 (MkObject lapi1) = MkObject $ \ff -> lapi1 $ \userstate api -> ff (t1t2 userstate) (fmap t1t2 api);
    };

    nonlockingObject :: userstate -> MutableEdit IO edit userstate -> Object edit userstate;
    nonlockingObject userstate api = MkObject $ \ff -> ff userstate api;

    freeObject :: forall edit. (Edit edit,FullReader (EditReader edit)) => EditSubject edit -> (EditSubject edit -> Bool) -> IO (Object edit ());
    freeObject firsta allowed = do
    {
        var <- newMVar firsta;
        let
        {
            lapi :: Object edit ();
            lapi = MkObject $ \ff -> modifyMVar var $ \olda -> do
            {
                let
                {
                    api :: MutableEdit (StateT (EditSubject edit) IO) edit ();
                    api = MkMutableEdit
                    {
                        mutableRead = readFromM $ get,
                        mutableEdit = \edits -> do
                        {
                            oa <- get;
                            let
                            {
                                na = fromReadFunction (applyEdits edits) oa;
                            };
                            return $ if allowed na then Just $ put na else Nothing;
                        }
                    };
                };
                (r,newa) <- runStateT (ff () api) olda;
                return (newa,r);
            };
        };
        return lapi;
    };

    mapObject :: forall f lensstate edita editb userstate. MonadOne f => FloatingEditLens' f lensstate edita editb -> Object edita (userstate,lensstate) -> Object editb userstate;
    mapObject lens@MkFloatingEditLens{..} (MkObject lapiA) = MkObject $ \(callB :: forall m. MonadIOInvert m => userstate -> MutableEdit m editb userstate -> m r) -> let
    {
        MkFloatingEditFunction{..} = floatingEditLensFunction;

        callA :: forall m. MonadIOInvert m => (userstate,lensstate) -> MutableEdit m edita (userstate,lensstate) -> m r;
        callA (firstuserstate,firstlensstate) (apiA :: MutableEdit m edita (userstate,lensstate)) = let
        {
            readA :: MutableRead m (EditReader edita);
            pushEditA :: [edita] -> m (Maybe (m (userstate,lensstate)));
            MkMutableEdit readA pushEditA = apiA;

            readB :: MutableRead (StateT lensstate m) (EditReader editb);
            readB rt = do
            {
                state <- get;
                lift $ mapMutableRead (floatingEditGet state) readA rt;
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

            pushEditB :: [editb] -> (StateT lensstate m) (Maybe (StateT lensstate m userstate));
            pushEditB editB = do
            {
                meditA <- convertEdit editB;
                case meditA of
                {
                    Nothing -> return $ Just $ return firstuserstate; -- is this correct?
                    Just editAs -> do
                    {
                        mstates <- lift $ pushEditA editAs;
                        return $ fmap (lift . fmap fst) mstates;
                    };
                };
            };

            apiB :: MutableEdit (StateT lensstate m) editb userstate;
            apiB = MkMutableEdit readB pushEditB;
        }
        in liftIOInvert $ \unlift -> do
        {
            (r,_newstate) <- unlift $ runStateT (callB firstuserstate apiB) firstlensstate; -- just throw away the new lens state: all these lens changes will be replayed by the update
            return r;
        };
    } in lapiA callA;

    pairObject :: Object ea ua -> Object eb ub -> Object (PairEdit ea eb) (ua,ub);
    pairObject (MkObject lapiA) (MkObject lapiB) = MkObject $ \ff ->
        lapiA $ \stateA meA -> liftIOInvert $ \unliftA -> StateT $ \oldsA ->
        lapiB $ \stateB meB -> liftIOInvert $ \unliftB -> StateT $ \oldsB ->
        fmap swap3 $ runStateT (ff (stateA,stateB) $ pairMutableEdit (remonadMutableEdit (stateFst . unliftA) meA) (remonadMutableEdit (stateSnd . unliftB) meB)) (oldsA,oldsB);
}
