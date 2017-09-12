module Truth.Core.Object.Object where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Whole;
    import Truth.Core.Types.None;
    import Truth.Core.Types.Pair;
    import Truth.Core.Object.MutableEdit;


    newtype Object edit = MkObject {runObject :: forall r. (forall m. IsStateIO m => MutableEdit m edit -> m r) -> IO r};

    nonlockingObject :: MutableEdit IO edit -> Object edit;
    nonlockingObject muted = MkObject $ \call -> call muted;

    noneObject :: Object (NoEdit (NoReader t));
    noneObject = nonlockingObject noneMutableEdit;

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

    mapObject :: forall lensstate edita editb. (Edit edita) =>
        (forall m. IsStateIO m => StateAccess m lensstate) -> EditLens lensstate edita editb -> Object edita -> Object editb;
    mapObject acc lens objectA = MkObject $ \callB -> runObject objectA $ \mutedA -> do
    {
        oldstate <- acc get;
        (r,_newstate) <- runStateT (callB $ mapMutableEdit lens mutedA) oldstate; -- ignore the new lens state: all these lens changes will be replayed by the update
        return r;
    };

    fixedMapObject :: forall edita editb. Edit edita => EditLens () edita editb -> Object edita -> Object editb;
    fixedMapObject lens object = mapObject unitStateAccess lens object;

    convertObject :: (EditSubject edita ~ EditSubject editb,FullEdit edita,FullEdit editb) => Object edita -> Object editb;
    convertObject = fixedMapObject convertEditLens;

    -- | Combines all the edits made in each call to the object.
    ;
    cacheObject :: Eq t => Object (WholeEdit t) -> Object (WholeEdit t);
    cacheObject (MkObject obj) = MkObject $ \call -> obj $ \muted -> do
    {
        oldval <- mutableRead muted ReadWhole;
        let
        {
            muted' = MkMutableEdit
            {
                mutableRead = \ReadWhole -> get,
                mutableEdit = singleAlwaysMutableEdit $ \(MkWholeEdit t) -> put t
            };
        };
        (r,newval) <- runStateT (call muted') oldval;
        if oldval == newval then return () else do
        {
            maction <- mutableEdit muted $ [MkWholeEdit newval];
            case maction of
            {
                Just action -> action;
                Nothing -> liftIO $ fail "disallowed cached edit";
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
