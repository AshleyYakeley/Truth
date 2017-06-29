module Truth.Core.Object.Object where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Whole;
    import Truth.Core.Types.Pair;
    import Truth.Core.Object.MutableEdit;


    newtype Object edit userstate = MkObject (forall r. (forall m. IsStateIO m => MutableEdit m edit userstate -> StateT userstate m r) -> IO r);

    lensObject :: Lens' Identity whole part -> Object edit whole -> Object edit part;
    lensObject lens (MkObject object) = MkObject $ \call -> object $ \muted -> lensStateT lens $ call $ fmap (lensGet lens) muted;

    nonlockingObject :: userstate -> MutableEdit IO edit userstate -> Object edit userstate;
    nonlockingObject userstate muted = MkObject $ \ff -> evalStateT (ff muted) userstate;

    isoObjectState :: (s1 -> s2,s2 -> s1) -> Object edit s1 -> Object edit s2;
    isoObjectState (s1s2,s2s1) (MkObject object) = MkObject $ \call -> object $ \muted -> isoStateT (s2s1,s1s2) $ call $ fmap s1s2 muted;

    mvarObject :: forall a. MVar a -> (a -> Bool) -> Object (WholeEdit a) ();
    mvarObject var allowed = MkObject $ \call -> modifyMVar var $ \olda -> do
    {
        let
        {
            muted :: MutableEdit (StateT a IO) (WholeEdit a) ();
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
        };
        (r,newa) <- runStateT (runUnitStateT (call muted)) olda;
        return (newa,r);
    };

    freeIOObject :: forall a. a -> (a -> Bool) -> IO (Object (WholeEdit a) ());
    freeIOObject firsta allowed = do
    {
        var <- newMVar firsta;
        return $ mvarObject var allowed;
    };

    floatingMapObject :: forall f lensstate edita editb userstate. MonadOne f => FloatingEditLens' f lensstate edita editb -> Object edita (userstate,lensstate) -> Object editb userstate;
    floatingMapObject lens@MkFloatingEditLens{..} (MkObject objectA) = MkObject $ \(callB :: forall m. IsStateIO m => MutableEdit m editb userstate -> StateT userstate m r) -> let
    {
        MkFloatingEditFunction{..} = floatingEditLensFunction;

        callA :: forall m. IsStateIO m => MutableEdit m edita (userstate,lensstate) -> StateT (userstate,lensstate) m r;
        callA (mutedA :: MutableEdit m edita (userstate,lensstate)) = let
        {
            readA :: MutableRead m (EditReader edita);
            pushEditA :: [edita] -> m (Maybe (m (userstate,lensstate)));
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

            pushEditB :: [editb] -> (StateT lensstate m) (Maybe (StateT lensstate m userstate));
            pushEditB editB = do
            {
                meditA <- convertEdit editB;
                case meditA of
                {
                    Nothing -> return Nothing;
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
        in joinStateT $ remonad revertStateT $ callB apiB; -- revert the new lens state: all these lens changes will be replayed by the update
    } in objectA callA;

    fixedMapObject :: forall f edita editb userstate. MonadOne f => EditLens' f edita editb -> Object edita userstate -> Object editb userstate;
    fixedMapObject lens object = floatingMapObject (fixedFloatingEditLens lens) $ isoObjectState (\s -> (s,()),fst) object;

    convertObject :: (EditSubject edita ~ EditSubject editb,FullEdit edita,FullEdit editb) => Object edita userstate -> Object editb userstate;
    convertObject = fixedMapObject @Identity convertEditLens;

    cacheObject :: Eq t => Object (WholeEdit t) () -> Object (WholeEdit t) ();
    cacheObject (MkObject obj) = MkObject $ \call -> obj $ \muted -> do
    {
        oldval <- lift $ mutableRead muted ReadWhole;
        let
        {
            muted' = MkMutableEdit
            {
                mutableRead = \ReadWhole -> get,
                mutableEdit = singleMutableEdit $ \(MkWholeEdit t) -> put t
            };
        };
        (r,newval) <- runStateT (swapStateT $ call $ muted') oldval;
        if oldval == newval then return () else do
        {
            maction <- lift $ mutableEdit muted $ [MkWholeEdit newval];
            case maction of
            {
                Just action -> lift action;
                Nothing -> fail "disallowed cached edit";
            };
        };
        return r;
    };

    map4 :: ((a,p),(b,q)) -> ((a,b),(p,q));
    map4 ((a,p),(b,q)) = ((a,b),(p,q));

    pairObject :: Object ea ua -> Object eb ub -> Object (PairEdit ea eb) (ua,ub);
    pairObject (MkObject objectA) (MkObject objectB) = MkObject $ \call ->
        objectA $ \mutedA -> mkStateIO $ \oldsA ->
        objectB $ \mutedB -> mkStateIO $ \oldsB ->
        fmap swap3 $ runStateT
        (isoStateT (map4,map4) $ joinStateT $ call $ pairMutableEdit (remonadMutableEdit (stateFst . fromStateIO) mutedA) (remonadMutableEdit (stateSnd . fromStateIO) mutedB))
        (oldsA,oldsB);
}
