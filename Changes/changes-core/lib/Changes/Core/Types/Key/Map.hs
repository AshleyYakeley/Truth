{-# OPTIONS -fno-warn-orphans #-}
module Changes.Core.Types.Key.Map where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Read
import Changes.Core.Types.Key.HasKey
import Changes.Core.Types.Key.Key
import Changes.Core.Types.None
import Changes.Core.Types.One.FullResult
import Changes.Core.Types.One.Read
import Changes.Core.Types.One.Result
import Changes.Core.Types.Partial
import Changes.Core.Types.ReadOnly
import Changes.Core.Types.Set
import Changes.Core.Types.Tuple.Function
import Changes.Core.Types.Tuple.Pair
import Changes.Core.Types.Tuple.Tuple
import Changes.Core.Types.Unit
import Changes.Core.Types.Whole

type KeyValueReader key valueUpdate = PairUpdateReader (ConstWholeUpdate key) valueUpdate

type KeyValueEdit key valueUpdate = PairUpdateEdit (ConstWholeUpdate key) valueUpdate

type KeyValueUpdate key valueUpdate = PairUpdate (ConstWholeUpdate key) valueUpdate

pattern ReadKey ::
    forall key valueUpdate t.
    () =>
    t ~ key =>
    KeyValueReader key valueUpdate t
pattern ReadKey = MkTupleUpdateReader SelectFirst ReadWhole

pattern ReadValue :: forall key valueUpdate t. UpdateReader valueUpdate t -> KeyValueReader key valueUpdate t
pattern ReadValue reader = MkTupleUpdateReader SelectSecond reader

{-# COMPLETE ReadKey, ReadValue #-}

instance Eq key => HasKeyReader (ListMap key value) (KeyValueReader key update) where
    readKey mr = mr ReadKey

-- Map values can change, but their keys are immutable.
instance Eq key => HasKeyUpdate (ListMap key value) (KeyValueUpdate key update) where
    updatesKey (MkTupleUpdate SelectFirst update) = never update
    updatesKey (MkTupleUpdate SelectSecond _) = Nothing

pattern MkKeyValueEdit :: UpdateEdit valueUpdate -> KeyValueEdit key valueUpdate
pattern MkKeyValueEdit e = MkTupleUpdateEdit SelectSecond e

pattern MkKeyValueUpdate :: valueUpdate -> KeyValueUpdate key valueUpdate
pattern MkKeyValueUpdate u = MkTupleUpdate SelectSecond u

type MapReader' key valueUpdate valueSubject = KeyReader (ListMap key valueSubject) (KeyValueReader key valueUpdate)

type MapReader key valueUpdate = MapReader' key valueUpdate (UpdateSubject valueUpdate)

pattern MapReadKeys ::
    forall key valueUpdate t.
    () =>
    t ~ ListSet key =>
    MapReader key valueUpdate t
pattern MapReadKeys = KeyReadKeys

pattern MapReadKey ::
    forall key valueUpdate t.
    () =>
    t ~ Maybe key =>
    key -> MapReader key valueUpdate t
pattern MapReadKey k = KeyReadItem k ReadKey

pattern MapReadValue ::
    forall key valueUpdate t.
    () =>
    forall a.
    t ~ Maybe a =>
    key -> UpdateReader valueUpdate a -> MapReader key valueUpdate t
pattern MapReadValue k rt = KeyReadItem k (ReadValue rt)

{-# COMPLETE MapReadKeys, MapReadKey, MapReadValue #-}

type MapEdit' key valueUpdate valueSubject = KeyEdit (ListMap key valueSubject) (KeyValueEdit key valueUpdate)

type MapEdit key valueUpdate = MapEdit' key valueUpdate (UpdateSubject valueUpdate)

pattern MapEditValue :: key -> UpdateEdit valueUpdate -> MapEdit key valueUpdate
pattern MapEditValue k e = KeyEditItem k (MkKeyValueEdit e)

pattern MapEditDelete :: key -> MapEdit key valueUpdate
pattern MapEditDelete k = KeyEditDelete k

pattern MapEditSet :: key -> UpdateSubject valueUpdate -> MapEdit key valueUpdate
pattern MapEditSet k v = KeyEditInsertReplace (k, v)

pattern MapEditClear :: MapEdit key valueUpdate
pattern MapEditClear = KeyEditClear

{-# COMPLETE MapEditValue, MapEditDelete, MapEditSet, MapEditClear #-}

type MapUpdate' key valueUpdate valueSubject = KeyUpdate (ListMap key valueSubject) (KeyValueUpdate key valueUpdate)

type MapUpdate key valueUpdate = MapUpdate' key valueUpdate (UpdateSubject valueUpdate)

pattern MapUpdateValue :: key -> valueUpdate -> MapUpdate key valueUpdate
pattern MapUpdateValue k u = KeyUpdateItem k (MkKeyValueUpdate u)

pattern MapUpdateDelete :: key -> MapUpdate key valueUpdate
pattern MapUpdateDelete k = KeyUpdateDelete k

pattern MapUpdateSet :: key -> UpdateSubject valueUpdate -> MapUpdate key valueUpdate
pattern MapUpdateSet k v = KeyUpdateInsertReplace (k, v)

pattern MapUpdateClear :: MapUpdate key valueUpdate
pattern MapUpdateClear = KeyUpdateClear

{-# COMPLETE MapUpdateValue, MapUpdateDelete, MapUpdateSet, MapUpdateClear #-}

wholeMapReadFunction ::
    forall valueUpdate cont.
    (IsMap cont, UpdateSubject valueUpdate ~ MapValue cont, SubjectReader (UpdateReader valueUpdate)) =>
    ReadFunction (WholeReader cont) (MapReader (ContainerKey cont) valueUpdate)
wholeMapReadFunction mr MapReadKeys = do
    smap <- mr ReadWhole
    return $ setFromList $ keys smap
wholeMapReadFunction mr (MapReadKey key) = do
    smap <- mr ReadWhole
    return $ do
        _ <- lookup key smap
        return key
wholeMapReadFunction mr (MapReadValue key rt) = do
    smap <- mr ReadWhole
    return $ do
        val <- lookup key smap
        return $ subjectToRead val rt

liftMapElementChangeLens ::
    forall key updateA updateB.
    ( Eq key
    , ApplicableEdit (UpdateEdit updateA)
    , FullSubjectReader (UpdateReader updateA)
    , FullSubjectReader (UpdateReader updateB)
    ) =>
    (UpdateSubject updateB -> IO (Maybe (UpdateSubject updateA))) ->
    ChangeLens updateA updateB ->
    ChangeLens (MapUpdate key updateA) (MapUpdate key updateB)
liftMapElementChangeLens bma l = let
    kbmka :: (key, UpdateSubject updateB) -> IO (Maybe (key, UpdateSubject updateA))
    kbmka (k, b) = do
        ma <- bma b
        return $ fmap (\a -> (k, a)) ma
    in liftKeyElementChangeLens kbmka (sndLiftChangeLens l)

-- | Transform and filter map values. Updates replace or delete only the
-- affected key, including when a value crosses the filter boundary.
mapMaybeMapChangeLens ::
    forall key a b.
    Eq key =>
    (a -> Maybe b) ->
    ChangeLens (MapUpdate key (WholeUpdate a)) (ReadOnlyUpdate (MapUpdate key (WholeUpdate b)))
mapMaybeMapChangeLens amb = let
    readValue :: forall m. MonadIO m => Readable m (MapReader key (WholeUpdate a)) -> key -> m (Maybe b)
    readValue mr k = fmap (>>= amb) $ mr $ MapReadValue k ReadWhole
    clRead :: ReadFunction (MapReader key (WholeUpdate a)) (MapReader key (WholeUpdate b))
    clRead mr MapReadKeys = do
        ks <- mr MapReadKeys
        fmap setFromList $ filterM (\k -> fmap isJust $ readValue mr k) $ setToList ks
    clRead mr (MapReadKey k) = fmap (fmap $ const k) $ readValue mr k
    clRead mr (MapReadValue k ReadWhole) = readValue mr k
    updateValue k mb = MkReadOnlyUpdate $ maybe (MapUpdateDelete k) (MapUpdateSet k) mb
    clUpdate ::
        forall m.
        MonadIO m =>
        MapUpdate key (WholeUpdate a) ->
        Readable m (MapReader key (WholeUpdate a)) ->
        m [ReadOnlyUpdate (MapUpdate key (WholeUpdate b))]
    clUpdate (MapUpdateValue k _) mr = fmap (pure . updateValue k) $ readValue mr k
    clUpdate (MapUpdateSet k a) _ = return [updateValue k $ amb a]
    clUpdate (MapUpdateDelete k) _ = return [MkReadOnlyUpdate $ MapUpdateDelete k]
    clUpdate MapUpdateClear _ = return [MkReadOnlyUpdate MapUpdateClear]
    in MkChangeLens{clPutEdits = clPutEditsNone, ..}

-- A given element of a map.
mapElementChangeLens :: forall key valueUpdate. Equivalence key -> key -> ChangeLens (MapUpdate key valueUpdate) (MaybeUpdate valueUpdate)
mapElementChangeLens eqv k = let
    clRead :: ReadFunction (MapReader key valueUpdate) (OneReader Maybe (UpdateReader valueUpdate))
    clRead mr ReadHasOne = fmap (fmap $ const ()) $ mr $ MapReadKey k
    clRead mr (ReadOne rt) = mr $ MapReadValue k rt
    clUpdate ::
        forall m.
        MonadIO m =>
        MapUpdate key valueUpdate ->
        Readable m (MapReader key valueUpdate) ->
        m [MaybeUpdate valueUpdate]
    clUpdate (MapUpdateValue key update) _
        | getEquivalence eqv key k = return [MkFullResultOneUpdate $ SuccessResultOneUpdate update]
    clUpdate (MapUpdateDelete key) _
        | getEquivalence eqv key k = return [MkFullResultOneUpdate $ NewResultOneUpdate Nothing]
    clUpdate (MapUpdateSet key _) _
        | getEquivalence eqv key k = return [MkFullResultOneUpdate $ NewResultOneUpdate $ Just ()]
    clUpdate MapUpdateClear _ = return [MkFullResultOneUpdate $ NewResultOneUpdate Nothing]
    clUpdate _ _ = return []
    clPutEdit ::
        forall m.
        MonadIO m =>
        MaybeEdit (UpdateEdit valueUpdate) ->
        m (Maybe [MapEdit key valueUpdate])
    clPutEdit (SuccessFullResultOneEdit edit) =
        return $ Just [MapEditValue k edit]
    clPutEdit (NewFullResultOneEdit Nothing) = return $ Just [MapEditDelete k]
    clPutEdit (NewFullResultOneEdit (Just value)) = return $ Just [MapEditSet k value]
    clPutEdits ::
        forall m.
        MonadIO m =>
        [MaybeEdit (UpdateEdit valueUpdate)] ->
        Readable m (MapReader key valueUpdate) ->
        m (Maybe [MapEdit key valueUpdate])
    clPutEdits = clPutEditsFromSimplePutEdit clPutEdit
    in MkChangeLens{..}

-- | Align the union of two maps. Writes replace both sides of an entry;
-- deleting an entry or clearing the map affects both source maps.
alignMapChangeLens ::
    forall key valueUpdateA valueUpdateB.
    ( Eq key
    , ApplicableEdit (UpdateEdit valueUpdateA)
    , ApplicableEdit (UpdateEdit valueUpdateB)
    , FullSubjectReader (UpdateReader valueUpdateA)
    , FullSubjectReader (UpdateReader valueUpdateB)
    ) =>
    ChangeLens
        (PairUpdate (MapUpdate key valueUpdateA) (MapUpdate key valueUpdateB))
        (MapUpdate key (WholeUpdate (These (UpdateSubject valueUpdateA) (UpdateSubject valueUpdateB))))
alignMapChangeLens = let
    readValue ::
        forall m.
        MonadIO m =>
        Readable m (PairUpdateReader (MapUpdate key valueUpdateA) (MapUpdate key valueUpdateB)) ->
        key ->
        m (Maybe (These (UpdateSubject valueUpdateA) (UpdateSubject valueUpdateB)))
    readValue mr k = do
        ma <- readMapValue (firstReadFunction mr) k
        mb <- readMapValue (secondReadFunction mr) k
        return $ case (ma, mb) of
            (Nothing, Nothing) -> Nothing
            (Just a, Nothing) -> Just $ This a
            (Nothing, Just b) -> Just $ That b
            (Just a, Just b) -> Just $ These a b
    clRead ::
        ReadFunction
            (PairUpdateReader (MapUpdate key valueUpdateA) (MapUpdate key valueUpdateB))
            (MapReader key (WholeUpdate (These (UpdateSubject valueUpdateA) (UpdateSubject valueUpdateB))))
    clRead mr MapReadKeys = liftA2 union (firstReadFunction mr MapReadKeys) (secondReadFunction mr MapReadKeys)
    clRead mr (MapReadKey k) = do
        ma <- firstReadFunction mr $ MapReadKey k
        mb <- secondReadFunction mr $ MapReadKey k
        return $ ma <|> mb
    clRead mr (MapReadValue k ReadWhole) = readValue mr k
    updateKey ::
        forall m.
        MonadIO m =>
        Readable m (PairUpdateReader (MapUpdate key valueUpdateA) (MapUpdate key valueUpdateB)) ->
        key ->
        m (MapUpdate key (WholeUpdate (These (UpdateSubject valueUpdateA) (UpdateSubject valueUpdateB))))
    updateKey mr k = do
        mv <- readValue mr k
        return $ maybe (MapUpdateDelete k) (MapUpdateSet k) mv
    clUpdate ::
        forall m.
        MonadIO m =>
        PairUpdate (MapUpdate key valueUpdateA) (MapUpdate key valueUpdateB) ->
        Readable m (PairUpdateReader (MapUpdate key valueUpdateA) (MapUpdate key valueUpdateB)) ->
        m [MapUpdate key (WholeUpdate (These (UpdateSubject valueUpdateA) (UpdateSubject valueUpdateB)))]
    clUpdate (MkTupleUpdate SelectFirst u) mr = alignUpdate u mr
    clUpdate (MkTupleUpdate SelectSecond u) mr = alignUpdate u mr
    alignUpdate ::
        forall m update.
        MonadIO m =>
        MapUpdate key update ->
        Readable m (PairUpdateReader (MapUpdate key valueUpdateA) (MapUpdate key valueUpdateB)) ->
        m [MapUpdate key (WholeUpdate (These (UpdateSubject valueUpdateA) (UpdateSubject valueUpdateB)))]
    alignUpdate MapUpdateClear mr = do
        ks <- clRead mr MapReadKeys
        us <- traverse (updateKey mr) $ setToList ks
        return $ MapUpdateClear : us
    alignUpdate (MapUpdateValue k _) mr = fmap pure $ updateKey mr k
    alignUpdate (MapUpdateDelete k) mr = fmap pure $ updateKey mr k
    alignUpdate (MapUpdateSet k _) mr = fmap pure $ updateKey mr k
    putValue k v = case v of
        This a -> [MkTupleUpdateEdit SelectFirst $ MapEditSet k a, MkTupleUpdateEdit SelectSecond $ MapEditDelete k]
        That b -> [MkTupleUpdateEdit SelectFirst $ MapEditDelete k, MkTupleUpdateEdit SelectSecond $ MapEditSet k b]
        These a b -> [MkTupleUpdateEdit SelectFirst $ MapEditSet k a, MkTupleUpdateEdit SelectSecond $ MapEditSet k b]
    clPutEdits ::
        forall m.
        MonadIO m =>
        [MapEdit key (WholeUpdate (These (UpdateSubject valueUpdateA) (UpdateSubject valueUpdateB)))] ->
        Readable m (PairUpdateReader (MapUpdate key valueUpdateA) (MapUpdate key valueUpdateB)) ->
        m (Maybe [PairUpdateEdit (MapUpdate key valueUpdateA) (MapUpdate key valueUpdateB)])
    clPutEdits = clPutEditsFromPutEdit $ \edit mr -> fmap Just $ case edit of
        MapEditValue k (MkWholeReaderEdit v) -> do
            mk <- clRead mr $ MapReadKey k
            return $ maybe [] (const $ putValue k v) mk
        MapEditSet k v -> return $ putValue k v
        MapEditDelete k -> return [MkTupleUpdateEdit SelectFirst $ MapEditDelete k, MkTupleUpdateEdit SelectSecond $ MapEditDelete k]
        MapEditClear -> return [MkTupleUpdateEdit SelectFirst MapEditClear, MkTupleUpdateEdit SelectSecond MapEditClear]
    in MkChangeLens{..}

-- | Rename keys bijectively, preserving values and their incremental changes.
bijectionMapKeysChangeLens :: forall a b update. Bijection a b -> ChangeLens (MapUpdate a update) (MapUpdate b update)
bijectionMapKeysChangeLens aba@(MkIsomorphism ab ba) = let
    mapKeyUpdate :: forall p q. (p -> q) -> MapUpdate p update -> MapUpdate q update
    mapKeyUpdate pq (MapUpdateValue p u) = MapUpdateValue (pq p) u
    mapKeyUpdate pq (MapUpdateDelete p) = MapUpdateDelete $ pq p
    mapKeyUpdate pq (MapUpdateSet p v) = MapUpdateSet (pq p) v
    mapKeyUpdate _ MapUpdateClear = MapUpdateClear
    mapKeyEdit :: forall p q. (p -> q) -> MapEdit p update -> MapEdit q update
    mapKeyEdit pq (MapEditValue p e) = MapEditValue (pq p) e
    mapKeyEdit pq (MapEditDelete p) = MapEditDelete $ pq p
    mapKeyEdit pq (MapEditSet p v) = MapEditSet (pq p) v
    mapKeyEdit _ MapEditClear = MapEditClear
    clRead :: ReadFunction (MapReader a update) (MapReader b update)
    clRead mra (MapReadValue b rt) = mra $ MapReadValue (ba b) rt
    clRead mra MapReadKeys = fmap (biIsoMap aba) $ mra MapReadKeys
    clRead mra (MapReadKey b) = fmap (fmap ab) $ mra $ MapReadKey (ba b)
    clUpdate ::
        forall m.
        MonadIO m =>
        MapUpdate a update ->
        Readable m (MapReader a update) ->
        m [MapUpdate b update]
    clUpdate ea _ = return $ pure $ mapKeyUpdate ab ea
    clPutEdits ::
        forall m.
        MonadIO m =>
        [MapEdit b update] ->
        Readable m (MapReader a update) ->
        m (Maybe [MapEdit a update])
    clPutEdits ebs _ = return $ Just $ fmap (mapKeyEdit ba) ebs
    in MkChangeLens{..}

-- | Combine maps under disjoint Left and Right keys. Clearing either source
-- rebuilds the result from the post-update readable to retain the other side.
mapCartesianSumChangeLens ::
    forall a b update.
    FullSubjectReader (UpdateReader update) =>
    ChangeLens (PairUpdate (MapUpdate a update) (MapUpdate b update)) (MapUpdate (Either a b) update)
mapCartesianSumChangeLens = let
    clRead :: ReadFunction (PairUpdateReader (MapUpdate a update) (MapUpdate b update)) (MapReader (Either a b) update)
    clRead mr (MapReadValue (Left a) rt) = firstReadFunction mr $ MapReadValue a rt
    clRead mr (MapReadValue (Right b) rt) = secondReadFunction mr $ MapReadValue b rt
    clRead mr MapReadKeys = do
        aa <- mr $ MkTupleUpdateReader SelectFirst MapReadKeys
        bb <- mr $ MkTupleUpdateReader SelectSecond MapReadKeys
        return $ aa <+++> bb
    clRead mr (MapReadKey (Left a)) = do
        ma' <- mr $ MkTupleUpdateReader SelectFirst $ MapReadKey a
        return $ fmap Left ma'
    clRead mr (MapReadKey (Right b)) = do
        mb' <- mr $ MkTupleUpdateReader SelectSecond $ MapReadKey b
        return $ fmap Right mb'
    clUpdate ::
        forall m.
        MonadIO m =>
        PairUpdate (MapUpdate a update) (MapUpdate b update) ->
        Readable m (PairUpdateReader (MapUpdate a update) (MapUpdate b update)) ->
        m [MapUpdate (Either a b) update]
    clUpdate (MkTupleUpdate SelectFirst (MapUpdateValue k u)) _ = return [MapUpdateValue (Left k) u]
    clUpdate (MkTupleUpdate SelectFirst (MapUpdateDelete v)) _ = return $ pure $ MapUpdateDelete $ Left v
    clUpdate (MkTupleUpdate SelectFirst (MapUpdateSet v value)) _ = return $ pure $ MapUpdateSet (Left v) value
    clUpdate (MkTupleUpdate SelectFirst MapUpdateClear) mr = getReplaceUpdates $ clRead mr
    clUpdate (MkTupleUpdate SelectSecond (MapUpdateValue k u)) _ = return [MapUpdateValue (Right k) u]
    clUpdate (MkTupleUpdate SelectSecond (MapUpdateDelete v)) _ = return $ pure $ MapUpdateDelete $ Right v
    clUpdate (MkTupleUpdate SelectSecond (MapUpdateSet v value)) _ =
        return $ pure $ MapUpdateSet (Right v) value
    clUpdate (MkTupleUpdate SelectSecond MapUpdateClear) mr = getReplaceUpdates $ clRead mr
    clPutEdits ::
        forall m.
        MonadIO m =>
        [MapEdit (Either a b) update] ->
        Readable m (PairUpdateReader (MapUpdate a update) (MapUpdate b update)) ->
        m (Maybe [PairUpdateEdit (MapUpdate a update) (MapUpdate b update)])
    clPutEdits =
        clPutEditsFromSimplePutEdit $ \case
            MapEditValue (Left k) e -> return $ Just [MkTupleUpdateEdit SelectFirst $ MapEditValue k e]
            MapEditValue (Right k) e -> return $ Just [MkTupleUpdateEdit SelectSecond $ MapEditValue k e]
            MapEditDelete (Left v) -> return $ Just $ pure $ MkTupleUpdateEdit SelectFirst $ MapEditDelete v
            MapEditDelete (Right v) -> return $ Just $ pure $ MkTupleUpdateEdit SelectSecond $ MapEditDelete v
            MapEditSet (Left v) value ->
                return $ Just $ pure $ MkTupleUpdateEdit SelectFirst $ MapEditSet v value
            MapEditSet (Right v) value ->
                return $ Just $ pure $ MkTupleUpdateEdit SelectSecond $ MapEditSet v value
            MapEditClear ->
                return
                    $ Just
                    $ [MkTupleUpdateEdit SelectFirst MapEditClear, MkTupleUpdateEdit SelectSecond MapEditClear]
    in MkChangeLens{..}

-- | Read a whole value only after checking membership. In particular, a
-- NoReader can produce its subject without performing any reads.
readMapValue ::
    forall key update m.
    (MonadIO m, FullSubjectReader (UpdateReader update)) =>
    Readable m (MapReader key update) -> key -> m (Maybe (UpdateSubject update))
readMapValue mr k = do
    mk <- mr $ MapReadKey k
    case mk of
        Nothing -> return Nothing
        Just _ -> unComposeInner $ readableToSubject $ \rt -> MkComposeInner $ mr $ MapReadValue k rt

-- | Rename and filter keys, leaving values unchanged. The codec must be
-- injective on accepted keys. Writes use its backwards mapping even when
-- the resulting source key is outside the visible subset.
codecMapKeysChangeLens ::
    forall a b update.
    Codec a b -> ChangeLens (MapUpdate a update) (MapUpdate b update)
codecMapKeysChangeLens codec@(MkCodec amb ba) = let
    clRead :: ReadFunction (MapReader a update) (MapReader b update)
    clRead mr MapReadKeys = fmap (injectiveFilter codec) $ mr MapReadKeys
    clRead mr (MapReadKey b) = fmap (injectiveFilter codec) $ mr $ MapReadKey (ba b)
    clRead mr (MapReadValue b rt) = case amb $ ba b of
        Nothing -> return Nothing
        Just _ -> mr $ MapReadValue (ba b) rt
    clUpdate ::
        forall m.
        MonadIO m =>
        MapUpdate a update -> Readable m (MapReader a update) -> m [MapUpdate b update]
    clUpdate (MapUpdateValue a u) _ = return $ maybe [] (\b -> [MapUpdateValue b u]) $ amb a
    clUpdate (MapUpdateDelete a) _ = return $ maybe [] (\b -> [MapUpdateDelete b]) $ amb a
    clUpdate (MapUpdateSet a v) _ = return $ maybe [] (\b -> [MapUpdateSet b v]) $ amb a
    clUpdate MapUpdateClear _ = return [MapUpdateClear]
    clPutEdits ::
        forall m.
        MonadIO m =>
        [MapEdit b update] -> Readable m (MapReader a update) -> m (Maybe [MapEdit a update])
    clPutEdits edits mr = fmap Just $ go [] edits
        where
            -- Include earlier insertions when translating a later clear.
            -- Repeated deletions are harmless; this needs no Eq constraint
            -- or ability to apply value edits to a reader.
            go :: [a] -> [MapEdit b update] -> m [MapEdit a update]
            go _ [] = return []
            go inserted (edit : rest) = do
                (newKeys, out) <- case edit of
                    MapEditValue b e -> return (inserted, [MapEditValue (ba b) e])
                    MapEditDelete b -> return (inserted, [MapEditDelete $ ba b])
                    MapEditSet b v -> return (ba b : inserted, [MapEditSet (ba b) v])
                    MapEditClear -> do
                        ks <- mr MapReadKeys
                        let visible = filter (isJust . amb) $ toList ks ++ inserted
                        return ([], fmap MapEditDelete visible)
                remaining <- go newKeys rest
                return $ out ++ remaining
    in MkChangeLens{..}

-- | As codecMapKeysChangeLens, but reject insertions and value edits outside
-- the visible subset, and ignore deletions outside it.
injectiveMapKeysChangeLens ::
    forall a b update.
    Codec a b -> ChangeLens (MapUpdate a update) (MapUpdate b update)
injectiveMapKeysChangeLens codec@(MkCodec amb ba) = let
    lens = codecMapKeysChangeLens @a @b @update codec
    visible = isJust . amb . ba
    prepare :: MapEdit b update -> Maybe [MapEdit b update]
    prepare e@(MapEditValue b _) = if visible b then Just [e] else Nothing
    prepare e@(MapEditSet b _) = if visible b then Just [e] else Nothing
    prepare e@(MapEditDelete b) = Just $ if visible b then [e] else []
    prepare MapEditClear = Just [MapEditClear]
    putEdits ::
        forall m.
        MonadIO m =>
        [MapEdit b update] -> Readable m (MapReader a update) -> m (Maybe [MapEdit a update])
    putEdits edits mr = case fmap concat $ traverse prepare edits of
        Nothing -> return Nothing
        Just es -> clPutEdits lens es mr
    in lens{clPutEdits = putEdits}

filterMapKeysChangeLens ::
    forall key update.
    (key -> Bool) -> ChangeLens (MapUpdate key update) (MapUpdate key update)
filterMapKeysChangeLens predicate = injectiveMapKeysChangeLens $ ifCodec predicate

collectMapKeysChangeLens ::
    forall a b update.
    FullSubjectReader (UpdateReader update) =>
    (b -> a) ->
    ChangeLens (PairUpdate (ROWUpdate (a -> Maybe b)) (MapUpdate a update)) (MapUpdate b update)
collectMapKeysChangeLens ba = bindChangeLens $ \amb -> injectiveMapKeysChangeLens $ MkCodec amb ba

-- | Filter by a changing predicate on keys. Predicate changes that expose an
-- entry publish its current whole value; unknown changes rebuild the filtered
-- result, not the source map.
filterMapUpdateFunction ::
    forall key update.
    FullSubjectReader (UpdateReader update) =>
    ChangeLens (PairUpdate (MapUpdate key update) (PartialSetUpdate key)) (ReadOnlyUpdate (MapUpdate key update))
filterMapUpdateFunction = let
    testKey ::
        forall m.
        MonadIO m =>
        Readable m (PairUpdateReader (MapUpdate key update) (PartialSetUpdate key)) -> key -> m Bool
    testKey mr k = secondReadFunction mr $ MkTupleUpdateReader (MkFunctionSelector k) ReadWhole
    clRead ::
        ReadFunction
            (PairUpdateReader (MapUpdate key update) (PartialSetUpdate key))
            (MapReader key update)
    clRead mr MapReadKeys = do
        ks <- firstReadFunction mr MapReadKeys
        ofilterM (testKey mr) ks
    clRead mr (MapReadKey k) = do
        visible <- testKey mr k
        if visible then firstReadFunction mr $ MapReadKey k else return Nothing
    clRead mr (MapReadValue k rt) = do
        visible <- testKey mr k
        if visible then firstReadFunction mr $ MapReadValue k rt else return Nothing
    forward ::
        forall m.
        MonadIO m =>
        key ->
        MapUpdate key update ->
        Readable m (PairUpdateReader (MapUpdate key update) (PartialSetUpdate key)) ->
        m [ReadOnlyUpdate (MapUpdate key update)]
    forward k u mr = do
        visible <- testKey mr k
        return $ if visible then [MkReadOnlyUpdate u] else []
    clUpdate ::
        forall m.
        MonadIO m =>
        PairUpdate (MapUpdate key update) (PartialSetUpdate key) ->
        Readable m (PairUpdateReader (MapUpdate key update) (PartialSetUpdate key)) ->
        m [ReadOnlyUpdate (MapUpdate key update)]
    clUpdate (MkTupleUpdate SelectFirst MapUpdateClear) _ = return [MkReadOnlyUpdate MapUpdateClear]
    clUpdate (MkTupleUpdate SelectFirst u@(MapUpdateValue k _)) mr = forward k u mr
    clUpdate (MkTupleUpdate SelectFirst u@(MapUpdateSet k _)) mr = forward k u mr
    clUpdate (MkTupleUpdate SelectFirst u@(MapUpdateDelete k)) mr = forward k u mr
    clUpdate (MkTupleUpdate SelectSecond (KnownPartialUpdate (MkTupleUpdate (MkFunctionSelector k) (MkWholeReaderUpdate visible)))) mr = do
        mv <- if visible then readMapValue (firstReadFunction mr) k else return Nothing
        return [MkReadOnlyUpdate $ maybe (MapUpdateDelete k) (MapUpdateSet k) mv]
    clUpdate (MkTupleUpdate SelectSecond (UnknownPartialUpdate _)) mr =
        fmap (fmap MkReadOnlyUpdate) $ getReplaceUpdates $ clRead mr
    in MkChangeLens{clPutEdits = clPutEditsNone, ..}
