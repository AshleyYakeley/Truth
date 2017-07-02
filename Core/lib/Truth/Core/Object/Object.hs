module Truth.Core.Object.Object where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Whole;
    import Truth.Core.Types.Pair;
    import Truth.Core.Object.MutableEdit;


    newtype Object edit = MkObject {runObject :: forall r. (forall m. IsStateIO m => MutableEdit m edit -> m r) -> IO r};

    nonlockingObject :: MutableEdit IO edit -> Object edit;
    nonlockingObject muted = MkObject $ \call -> call muted;

    mvarObject :: forall a. MVar a -> (a -> Bool) -> Object (WholeEdit a);
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
    } in call muted;

    freeIOObject :: forall a. a -> (a -> Bool) -> IO (Object (WholeEdit a));
    freeIOObject firsta allowed = do
    {
        var <- newMVar firsta;
        return $ mvarObject var allowed;
    };

    floatingMapObject :: forall f lensstate edita editb. (MonadOne f,Edit edita) => (forall m. IsStateIO m => StateAccess m lensstate) -> FloatingEditLens' f lensstate edita editb -> Object edita -> Object editb;
    floatingMapObject acc lens@MkFloatingEditLens{..} (MkObject objectA) = MkObject $ \(callB :: forall m. IsStateIO m => MutableEdit m editb -> m r) -> let
    {
        MkFloatingEditFunction{..} = floatingEditLensFunction;

        callA :: forall m. IsStateIO m => MutableEdit m edita -> m r;
        callA (mutedA :: MutableEdit m edita) = let
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
        }
        in do
        {
            oldstate <- acc get;
            (r,_newstate) <- runStateT (callB apiB) oldstate; -- ignore the new lens state: all these lens changes will be replayed by the update
            return r;
        };
    } in objectA callA;

    fixedMapObject :: forall f edita editb. (MonadOne f,Edit edita) => EditLens' f edita editb -> Object edita -> Object editb;
    fixedMapObject lens object = floatingMapObject unitStateAccess (fixedFloatingEditLens lens) object;

    convertObject :: (EditSubject edita ~ EditSubject editb,FullEdit edita,FullEdit editb) => Object edita -> Object editb;
    convertObject = fixedMapObject @Identity convertEditLens;

    -- | Combines all the edits made in each call to the object.
    cacheObject :: Eq t => Object (WholeEdit t) -> Object (WholeEdit t);
    cacheObject (MkObject obj) = MkObject $ \call -> obj $ \muted -> do
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
        (r,newval) <- runStateT (call muted') oldval;
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

    pairObject :: Object ea -> Object eb -> Object (PairEdit ea eb);
    pairObject (MkObject objectA) (MkObject objectB) = MkObject $ \call ->
        objectA $ \mutedA -> mkStateIO $ \oldsA ->
        objectB $ \mutedB -> mkStateIO $ \oldsB ->
        fmap swap3 $ runStateT
        (let
        {
            mutedA' = remonadMutableEdit (stateFst . fromStateIO) mutedA;
            mutedB' = remonadMutableEdit (stateSnd . fromStateIO) mutedB;
            mutedAB = pairMutableEdit mutedA' mutedB';
        } in call mutedAB)
        (oldsA,oldsB);
}
