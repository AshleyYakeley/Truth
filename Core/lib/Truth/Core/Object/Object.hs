module Truth.Core.Object.Object where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Whole;
    import Truth.Core.Types.Pair;
    import Truth.Core.Object.MutableEdit;


    newtype Object edit userstate = MkObject {runObject :: forall r. (forall m. IsStateIO m => MutableEdit m edit -> StateAccess m userstate -> m r) -> IO r};

    lensObject :: Lens' Identity whole part -> Object edit whole -> Object edit part;
    lensObject lens (MkObject object) = MkObject $ \call -> object $ \muted sacc -> call muted $ lensStateAccess lens sacc;

    nonlockingObject :: MutableEdit IO edit -> Object edit ();
    nonlockingObject muted = MkObject $ \call -> call muted unitStateAccess;

    isoObjectState :: (s1 -> s2,s2 -> s1) -> Object edit s1 -> Object edit s2;
    isoObjectState (s1s2,s2s1) = lensObject $ MkLens s1s2 $ \s2 _ -> Identity $ s2s1 s2;

    mvarObject :: forall a. MVar a -> (a -> Bool) -> Object (WholeEdit a) ();
    mvarObject var allowed = MkObject $ \call -> mvarStateAccess var $ let
    {
        muted :: MutableEdit (StateT a IO) (WholeEdit a);
        muted = let
        {
            mutableRead :: MutableRead (StateT a IO) (WholeReader a);
            mutableRead ReadWhole = get;

            mutableEdit edits = do
            {
                na <- fromReadFunctionM (applyEdits edits) get;
                return $ if allowed na then Just $ put na else Nothing;
            };
        } in MkMutableEdit{..};
    } in call muted unitStateAccess;

    freeIOObject :: forall a. a -> (a -> Bool) -> IO (Object (WholeEdit a) ());
    freeIOObject firsta allowed = do
    {
        var <- newMVar firsta;
        return $ mvarObject var allowed;
    };

    floatingMapObject :: forall f lensstate edita editb userstate. (MonadOne f,Edit edita) => FloatingEditLens' f lensstate edita editb -> Object edita (userstate,lensstate) -> Object editb userstate;
    floatingMapObject lens@MkFloatingEditLens{..} (MkObject objectA) = MkObject $ \(callB :: forall m. IsStateIO m => MutableEdit m editb -> StateAccess m userstate -> m r) -> let
    {
        MkFloatingEditFunction{..} = floatingEditLensFunction;

        callA :: forall m. IsStateIO m => MutableEdit m edita -> StateAccess m (userstate,lensstate) -> m r;
        callA (mutedA :: MutableEdit m edita) accA = let
        {
            readA :: MutableRead m (EditReader edita);
            pushEditA :: [edita] -> m (Maybe (m ()));
            MkMutableEdit readA pushEditA = mutedA;

            readB :: MutableRead (StateT lensstate m) (EditReader editb);
            readB rt = do
            {
                st <- get;
                lift $ mapMutableRead (floatingEditGet st) readA rt;
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

            pushEditB :: [editb] -> (StateT lensstate m) (Maybe (StateT lensstate m ()));
            pushEditB editB = do
            {
                meditA <- convertEdit editB;
                case meditA of
                {
                    Nothing -> return Nothing;
                    Just editAs -> do
                    {
                        mstates <- lift $ pushEditA editAs;
                        return $ fmap lift mstates;
                    };
                };
            };

            apiB :: MutableEdit (StateT lensstate m) editb;
            apiB = MkMutableEdit readB pushEditB;

            accB :: StateAccess (StateT lensstate m) userstate;
            accB = splitStateAccess accA;
        }
        in do
        {
            (_,oldstate) <- accA get;
            (r,_newstate) <- runStateT (callB apiB accB) oldstate; -- ignore the new lens state: all these lens changes will be replayed by the update
            return r;
        };
    } in objectA callA;

    fixedMapObject :: forall f edita editb userstate. (MonadOne f,Edit edita) => EditLens' f edita editb -> Object edita userstate -> Object editb userstate;
    fixedMapObject lens object = floatingMapObject (fixedFloatingEditLens lens) $ isoObjectState (\s -> (s,()),fst) object;

    convertObject :: (EditSubject edita ~ EditSubject editb,FullEdit edita,FullEdit editb) => Object edita userstate -> Object editb userstate;
    convertObject = fixedMapObject @Identity convertEditLens;

    cacheObject :: Eq t => Object (WholeEdit t) () -> Object (WholeEdit t) ();
    cacheObject (MkObject obj) = MkObject $ \call -> obj $ \muted acc -> do
    {
        oldval <- mutableRead muted ReadWhole;
        let
        {
            muted' = MkMutableEdit
            {
                mutableRead = \ReadWhole -> get,
                mutableEdit = singleMutableEdit $ \(MkWholeEdit t) -> put t
            };
        };
        (r,newval) <- runStateT (call muted' $ liftStateAccess acc) oldval;
        if oldval == newval then return () else do
        {
            maction <- mutableEdit muted $ [MkWholeEdit newval];
            case maction of
            {
                Just action -> action;
                Nothing -> fail "disallowed cached edit";
            };
        };
        return r;
    };

    pairObject :: Object ea ua -> Object eb ub -> Object (PairEdit ea eb) (ua,ub);
    pairObject (MkObject objectA) (MkObject objectB) = MkObject $ \call ->
        objectA $ \mutedA accA -> mkStateIO $ \oldsA ->
        objectB $ \mutedB accB -> mkStateIO $ \oldsB ->
        fmap swap3 $ runStateT
        (let
        {
            mutedA' = remonadMutableEdit (stateFst . fromStateIO) mutedA;
            mutedB' = remonadMutableEdit (stateSnd . fromStateIO) mutedB;
            mutedAB = pairMutableEdit mutedA' mutedB';

            remonadA :: forall m ss s. IsStateIO m => StateAccess m s -> StateAccess (StateT (IOState m,ss) IO) s;
            remonadA sta st = joinStateT $ swapStateT $ remonad (fromStateIO . sta . remonad toStateIO) $ swapStateT $ remonad (swapStateT . splitStateT) st;

            remonadB :: forall m ss s. IsStateIO m => StateAccess m s -> StateAccess (StateT (ss,IOState m) IO) s;
            remonadB sta st = joinStateT $ remonad (fromStateIO . sta . remonad toStateIO) $ swapStateT $ remonad splitStateT st;
        } in call mutedAB $ pairStateAccess (remonadA accA) (remonadB accB))
        (oldsA,oldsB);
}
