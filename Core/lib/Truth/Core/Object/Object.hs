module Truth.Core.Object.Object where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Whole;
    import Truth.Core.Types.Pair;
    import Truth.Core.Object.MutableEdit;


    newtype Object edit userstate = MkObject (forall r. (forall m. MonadIOInvert m => userstate -> MutableEdit m edit userstate -> m r) -> IO r);

    instance Functor (Object edit) where
    {
        fmap t1t2 (MkObject lapi1) = MkObject $ \ff -> lapi1 $ \userstate muted -> ff (t1t2 userstate) (fmap t1t2 muted);
    };

    nonlockingObject :: userstate -> MutableEdit IO edit userstate -> Object edit userstate;
    nonlockingObject userstate muted = MkObject $ \ff -> ff userstate muted;

    freeObject :: forall edit. (Edit edit,FullReader (EditReader edit)) => EditSubject edit -> (EditSubject edit -> Bool) -> IO (Object edit ());
    freeObject firsta allowed = do
    {
        var <- newMVar firsta;
        return $ MkObject $ \ff -> modifyMVar var $ \olda -> do
        {
            let
            {
                muted :: MutableEdit (StateT (EditSubject edit) IO) edit ();
                muted = MkMutableEdit
                {
                    mutableRead = readFromM $ get,
                    mutableEdit = \edits -> do
                    {
                        na <- fromReadFunctionM (applyEdits edits) get;
                        return $ if allowed na then Just $ put na else Nothing;
                    }
                };
            };
            (r,newa) <- runStateT (ff () muted) olda;
            return (newa,r);
        };
    };

    floatingMapObject :: forall f lensstate edita editb userstate. MonadOne f => FloatingEditLens' f lensstate edita editb -> Object edita (userstate,lensstate) -> Object editb userstate;
    floatingMapObject lens@MkFloatingEditLens{..} (MkObject objectA) = MkObject $ \(callB :: forall m. MonadIOInvert m => userstate -> MutableEdit m editb userstate -> m r) -> let
    {
        MkFloatingEditFunction{..} = floatingEditLensFunction;

        callA :: forall m. MonadIOInvert m => (userstate,lensstate) -> MutableEdit m edita (userstate,lensstate) -> m r;
        callA (firstuserstate,firstlensstate) (mutedA :: MutableEdit m edita (userstate,lensstate)) = let
        {
            readA :: MutableRead m (EditReader edita);
            pushEditA :: [edita] -> m (Maybe (m (userstate,lensstate)));
            MkMutableEdit readA pushEditA = mutedA;

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
    } in objectA callA;

    fixedMapObject :: forall f edita editb userstate. MonadOne f => EditLens' f edita editb -> Object edita userstate -> Object editb userstate;
    fixedMapObject lens object = floatingMapObject (fixedFloatingEditLens lens) $ fmap (\s -> (s,())) object;

    convertObject :: (EditSubject edita ~ EditSubject editb,FullEdit edita,FullEdit editb) => Object edita userstate -> Object editb userstate;
    convertObject = fixedMapObject @Identity convertEditLens;

    cacheObject :: Eq t => Object (WholeEdit t) () -> Object (WholeEdit t) ();
    cacheObject (MkObject obj) = MkObject $ \ff -> obj $ \s me -> do
    {
        oldval <- mutableRead me ReadWhole;
        let
        {
            me' = MkMutableEdit
            {
                mutableRead = \ReadWhole -> get,
                mutableEdit = singleMutableEdit $ \(MkWholeEdit t) -> put t
            };
        };
        (r,newval) <- runStateT (ff s me') oldval;
        if oldval == newval then return () else do
        {
            maction <- mutableEdit me $ [MkWholeEdit newval];
            case maction of
            {
                Just action -> action;
                Nothing -> fail "disallowed cached edit";
            };
        };
        return r;
    };

    pairObject :: Object ea ua -> Object eb ub -> Object (PairEdit ea eb) (ua,ub);
    pairObject (MkObject objectA) (MkObject lapiB) = MkObject $ \ff ->
        objectA $ \stateA meA -> liftIOInvert $ \unliftA -> StateT $ \oldsA ->
        lapiB $ \stateB meB -> liftIOInvert $ \unliftB -> StateT $ \oldsB ->
        fmap swap3 $ runStateT (ff (stateA,stateB) $ pairMutableEdit (remonadMutableEdit (stateFst . unliftA) meA) (remonadMutableEdit (stateSnd . unliftB) meB)) (oldsA,oldsB);
}
