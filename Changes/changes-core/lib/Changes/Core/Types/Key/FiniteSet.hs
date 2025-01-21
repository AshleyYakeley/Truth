{-# OPTIONS -fno-warn-orphans #-}

module Changes.Core.Types.Key.FiniteSet where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Read
import Changes.Core.Types.Key.Key
import Changes.Core.Types.Lattice
import Changes.Core.Types.None
import Changes.Core.Types.Partial
import Changes.Core.Types.ReadOnly
import Changes.Core.Types.Set
import Changes.Core.Types.Tuple.Function
import Changes.Core.Types.Tuple.Pair
import Changes.Core.Types.Tuple.Tuple
import Changes.Core.Types.Unit
import Changes.Core.Types.Whole

{- equivalent to:
data FiniteSetReader subj t where
    KeyReadKeys :: FiniteSetReader subj (ListSet subj)
    KeyReadItem :: subj -> WholeReader subj t -> FiniteSetReader subj (Maybe t)
-}
type FiniteSetReader subj = KeyReader (ListSet subj) (WholeReader subj)

{- equivalent to:
data FiniteSetEdit subj where
    KeyEditItem :: subj -> ConstWholeEdit subj -> FiniteSetEdit subj
    KeyEditDelete :: subj -> FiniteSetEdit subj
    KeyEditInsertReplace :: subj -> FiniteSetEdit subj
    KeyEditClear :: FiniteSetEdit subj
-}
type FiniteSetEdit subj = KeyEdit (ListSet subj) (ConstWholeEdit subj)

{- equivalent to:
data FiniteSetUpdate subj where
    KeyUpdateItem :: subj -> ConstWholeUpdate subj -> FiniteSetUpdate subj
    KeyUpdateDelete :: subj -> FiniteSetUpdate subj
    KeyUpdateInsertReplace :: subj -> FiniteSetUpdate subj
    KeyUpdateClear :: FiniteSetUpdate subj
-}
type FiniteSetUpdate subj = KeyUpdate (ListSet subj) (ConstWholeUpdate subj)

wholeFiniteSetReadFunction :: Eq subj => ReadFunction (WholeReader (ListSet subj)) (FiniteSetReader subj)
wholeFiniteSetReadFunction mr KeyReadKeys = mr ReadWhole
wholeFiniteSetReadFunction mr (KeyReadItem subj ReadWhole) = do
    fset <- mr ReadWhole
    return
        $ if member subj fset
            then Just subj
            else Nothing

finiteSetChangeLens :: forall subj. Equivalence subj -> subj -> ChangeLens (FiniteSetUpdate subj) (WholeUpdate Bool)
finiteSetChangeLens eqv subj = let
    clRead :: ReadFunction (FiniteSetReader subj) (WholeReader Bool)
    clRead mr ReadWhole = fmap isJust $ mr $ KeyReadItem subj ReadWhole
    clUpdate ::
        forall m.
        MonadIO m =>
        FiniteSetUpdate subj ->
        Readable m (FiniteSetReader subj) ->
        m [WholeUpdate Bool]
    clUpdate (KeyUpdateItem _ update) _ = never update
    clUpdate (KeyUpdateDelete key) _
        | equivalent eqv key subj = return [MkWholeReaderUpdate False]
    clUpdate (KeyUpdateDelete _) _ = return []
    clUpdate (KeyUpdateInsertReplace key) _
        | equivalent eqv key subj = return [MkWholeReaderUpdate True]
    clUpdate (KeyUpdateInsertReplace _) _ = return []
    clUpdate KeyUpdateClear _ = return [MkWholeReaderUpdate False]
    clPutEdit ::
        forall m.
        MonadIO m =>
        WholeEdit Bool ->
        m (Maybe [FiniteSetEdit subj])
    clPutEdit (MkWholeReaderEdit False) = return $ Just [KeyEditDelete subj]
    clPutEdit (MkWholeReaderEdit True) = return $ Just [KeyEditInsertReplace subj]
    clPutEdits ::
        forall m.
        MonadIO m =>
        [WholeEdit Bool] ->
        Readable m (FiniteSetReader subj) ->
        m (Maybe [FiniteSetEdit subj])
    clPutEdits = clPutEditsFromSimplePutEdit clPutEdit
    in MkChangeLens{..}

instance Eq subj => JoinSemiLatticeReadOnlyChangeLens (FiniteSetUpdate subj) where
    joinChangeLens = let
        clRead :: ReadFunction (PairUpdateReader (FiniteSetUpdate subj) (FiniteSetUpdate subj)) (FiniteSetReader subj)
        clRead mr KeyReadKeys = do
            keys1 <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
            keys2 <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
            return $ keys1 \/ keys2
        clRead mr (KeyReadItem item ReadWhole) = do
            (isJust -> r1) <- mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem item ReadWhole
            (isJust -> r2) <- mr $ MkTupleUpdateReader SelectSecond $ KeyReadItem item ReadWhole
            return
                $ if r1 \/ r2
                    then Just item
                    else Nothing
        clUpdate ::
            forall m.
            MonadIO m =>
            PairUpdate (FiniteSetUpdate subj) (FiniteSetUpdate subj) ->
            Readable m (PairUpdateReader (FiniteSetUpdate subj) (FiniteSetUpdate subj)) ->
            m [ReadOnlyUpdate (FiniteSetUpdate subj)]
        clUpdate (MkTupleUpdate SelectFirst (KeyUpdateItem _ edit)) _ = never edit
        clUpdate (MkTupleUpdate SelectFirst (KeyUpdateDelete item)) mr = do
            (isJust -> r2) <- mr $ MkTupleUpdateReader SelectSecond $ KeyReadItem item ReadWhole
            return
                $ if r2
                    then []
                    else [MkReadOnlyUpdate $ KeyUpdateDelete item]
        clUpdate (MkTupleUpdate SelectFirst (KeyUpdateInsertReplace item)) mr = do
            (isJust -> r2) <- mr $ MkTupleUpdateReader SelectSecond $ KeyReadItem item ReadWhole
            return
                $ if r2
                    then []
                    else [MkReadOnlyUpdate $ KeyUpdateInsertReplace item]
        clUpdate (MkTupleUpdate SelectFirst KeyUpdateClear) mr = do
            keys2 <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
            return $ fmap MkReadOnlyUpdate $ KeyUpdateClear : (fmap KeyUpdateInsertReplace $ toList keys2)
        clUpdate (MkTupleUpdate SelectSecond (KeyUpdateItem _ edit)) _ = never edit
        clUpdate (MkTupleUpdate SelectSecond (KeyUpdateDelete item)) mr = do
            (isJust -> r1) <- mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem item ReadWhole
            return
                $ if r1
                    then []
                    else [MkReadOnlyUpdate $ KeyUpdateDelete item]
        clUpdate (MkTupleUpdate SelectSecond (KeyUpdateInsertReplace item)) mr = do
            (isJust -> r1) <- mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem item ReadWhole
            return
                $ if r1
                    then []
                    else [MkReadOnlyUpdate $ KeyUpdateInsertReplace item]
        clUpdate (MkTupleUpdate SelectSecond KeyUpdateClear) mr = do
            keys1 <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
            return $ fmap MkReadOnlyUpdate $ KeyUpdateClear : (fmap KeyUpdateInsertReplace $ toList keys1)
        in MkChangeLens{clPutEdits = clPutEditsNone, ..}

instance Eq subj => MeetSemiLatticeReadOnlyChangeLens (FiniteSetUpdate subj) where
    meetChangeLens = let
        clRead :: ReadFunction (PairUpdateReader (FiniteSetUpdate subj) (FiniteSetUpdate subj)) (FiniteSetReader subj)
        clRead mr KeyReadKeys = do
            keys1 <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
            keys2 <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
            return $ keys1 /\ keys2
        clRead mr (KeyReadItem item ReadWhole) = do
            (isJust -> r1) <- mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem item ReadWhole
            (isJust -> r2) <- mr $ MkTupleUpdateReader SelectSecond $ KeyReadItem item ReadWhole
            return
                $ if r1 /\ r2
                    then Just item
                    else Nothing
        clUpdate ::
            forall m.
            MonadIO m =>
            PairUpdate (FiniteSetUpdate subj) (FiniteSetUpdate subj) ->
            Readable m (PairUpdateReader (FiniteSetUpdate subj) (FiniteSetUpdate subj)) ->
            m [ReadOnlyUpdate (FiniteSetUpdate subj)]
        clUpdate (MkTupleUpdate SelectFirst (KeyUpdateItem _ edit)) _ = never edit
        clUpdate (MkTupleUpdate SelectFirst (KeyUpdateDelete item)) mr = do
            (isJust -> r2) <- mr $ MkTupleUpdateReader SelectSecond $ KeyReadItem item ReadWhole
            return
                $ if r2
                    then [MkReadOnlyUpdate $ KeyUpdateDelete item]
                    else []
        clUpdate (MkTupleUpdate SelectFirst (KeyUpdateInsertReplace item)) mr = do
            (isJust -> r2) <- mr $ MkTupleUpdateReader SelectSecond $ KeyReadItem item ReadWhole
            return
                $ if r2
                    then [MkReadOnlyUpdate $ KeyUpdateInsertReplace item]
                    else []
        clUpdate (MkTupleUpdate SelectFirst KeyUpdateClear) mr = do
            keys2 <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
            return
                $ if null keys2
                    then []
                    else [MkReadOnlyUpdate KeyUpdateClear]
        clUpdate (MkTupleUpdate SelectSecond (KeyUpdateItem _ edit)) _ = never edit
        clUpdate (MkTupleUpdate SelectSecond (KeyUpdateDelete item)) mr = do
            (isJust -> r1) <- mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem item ReadWhole
            return
                $ if r1
                    then [MkReadOnlyUpdate $ KeyUpdateDelete item]
                    else []
        clUpdate (MkTupleUpdate SelectSecond (KeyUpdateInsertReplace item)) mr = do
            (isJust -> r1) <- mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem item ReadWhole
            return
                $ if r1
                    then [MkReadOnlyUpdate $ KeyUpdateInsertReplace item]
                    else []
        clUpdate (MkTupleUpdate SelectSecond KeyUpdateClear) mr = do
            keys1 <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
            return
                $ if null keys1
                    then []
                    else [MkReadOnlyUpdate KeyUpdateClear]
        in MkChangeLens{clPutEdits = clPutEditsNone, ..}

injectiveMapFiniteSetChangeLens :: forall a b. Codec a b -> ChangeLens (FiniteSetUpdate a) (FiniteSetUpdate b)
injectiveMapFiniteSetChangeLens codec@(MkCodec amb ba) = let
    mapFiniteSetEdit ::
        forall m.
        MonadIO m =>
        Readable m (FiniteSetReader a) ->
        FiniteSetEdit b ->
        m (Maybe [FiniteSetEdit a])
    mapFiniteSetEdit _ (KeyEditItem _ edit) = return $ Just [never edit]
    mapFiniteSetEdit _ (KeyEditDelete b) =
        return
            $ Just
            $ let
                a = ba b
                in case amb a of
                    Nothing -> []
                    Just _ -> [KeyEditDelete a]
    mapFiniteSetEdit _ (KeyEditInsertReplace b) =
        return $ do
            let a = ba b
            _ <- amb a
            return [KeyEditInsertReplace a]
    mapFiniteSetEdit rma KeyEditClear = do
        aa <- rma KeyReadKeys
        let
            mapa :: a -> Maybe (FiniteSetEdit a)
            mapa a = do
                _ <- amb a
                return $ KeyEditDelete a
        return $ Just $ mapMaybe mapa $ toList aa
    clRead :: ReadFunction (FiniteSetReader a) (FiniteSetReader b)
    clRead mra KeyReadKeys = fmap (injectiveFilter codec) $ mra KeyReadKeys
    clRead mra (KeyReadItem b ReadWhole) = fmap (injectiveFilter codec) $ mra $ KeyReadItem (ba b) ReadWhole
    clUpdate ::
        forall m.
        MonadIO m =>
        FiniteSetUpdate a ->
        Readable m (FiniteSetReader a) ->
        m [FiniteSetUpdate b]
    clUpdate (KeyUpdateItem _ update) _ = never update
    clUpdate (KeyUpdateDelete a) _
        | Just b <- amb a = return $ pure $ KeyUpdateDelete b
    clUpdate (KeyUpdateInsertReplace a) _
        | Just b <- amb a = return $ pure $ KeyUpdateInsertReplace b
    clUpdate KeyUpdateClear _ = return $ pure KeyUpdateClear
    clUpdate _ _ = return []
    clPutEdits ::
        forall m.
        MonadIO m =>
        [FiniteSetEdit b] ->
        Readable m (FiniteSetReader a) ->
        m (Maybe [FiniteSetEdit a])
    clPutEdits ebs rma = fmap mconcat $ for ebs $ mapFiniteSetEdit rma
    in MkChangeLens{..}

collectFiniteSetChangeLens ::
    (b -> a) -> ChangeLens (PairUpdate (ROWUpdate (a -> Maybe b)) (FiniteSetUpdate a)) (FiniteSetUpdate b)
collectFiniteSetChangeLens ba = bindChangeLens $ \amb -> injectiveMapFiniteSetChangeLens $ MkCodec amb ba

filterFiniteSetChangeLens :: forall a. (a -> Bool) -> ChangeLens (FiniteSetUpdate a) (FiniteSetUpdate a)
filterFiniteSetChangeLens f = injectiveMapFiniteSetChangeLens $ ifCodec f

codecFiniteSetChangeLens ::
    forall a b.
    Eq a =>
    Codec a b ->
    ChangeLens (FiniteSetUpdate a) (FiniteSetUpdate b)
codecFiniteSetChangeLens codec@(MkCodec amb ba) = let
    clRead :: ReadFunction (FiniteSetReader a) (FiniteSetReader b)
    clRead mra KeyReadKeys = fmap (injectiveFilter codec) $ mra KeyReadKeys
    clRead mra (KeyReadItem b ReadWhole) = fmap (injectiveFilter codec) $ mra $ KeyReadItem (ba b) ReadWhole
    clUpdate ::
        forall m.
        MonadIO m =>
        FiniteSetUpdate a ->
        Readable m (FiniteSetReader a) ->
        m [FiniteSetUpdate b]
    clUpdate (KeyUpdateItem _ update) _ = never update
    clUpdate (KeyUpdateDelete a) _ =
        return
            $ case amb a of
                Nothing -> []
                Just b -> [KeyUpdateDelete b]
    clUpdate (KeyUpdateInsertReplace a) _ =
        return
            $ case amb a of
                Nothing -> []
                Just b -> [KeyUpdateInsertReplace b]
    clUpdate KeyUpdateClear _ = return [KeyUpdateClear]
    clPutEdit ::
        forall m.
        MonadIO m =>
        FiniteSetEdit b ->
        Readable m (FiniteSetReader a) ->
        m (Maybe [FiniteSetEdit a])
    clPutEdit (KeyEditItem _ edit) _ = never edit
    clPutEdit (KeyEditDelete b) _ = return $ Just [KeyEditDelete $ ba b]
    clPutEdit (KeyEditInsertReplace b) _ = return $ Just [KeyEditInsertReplace $ ba b]
    clPutEdit KeyEditClear mra = do
        fsa <- mra KeyReadKeys
        let
            ff :: a -> Maybe (FiniteSetEdit a)
            ff a = do
                _ <- amb a
                return $ KeyEditDelete a
        return $ Just $ mapMaybe ff $ toList fsa
    clPutEdits ::
        forall m.
        MonadIO m =>
        [FiniteSetEdit b] ->
        Readable m (FiniteSetReader a) ->
        m (Maybe [FiniteSetEdit a])
    clPutEdits = clPutEditsFromPutEdit clPutEdit
    in MkChangeLens{..}

bijectionFiniteSetChangeLens :: forall a b. Bijection a b -> ChangeLens (FiniteSetUpdate a) (FiniteSetUpdate b)
bijectionFiniteSetChangeLens aba@(MkIsomorphism ab ba) = let
    mapFiniteSetUpdate :: forall p q. (p -> q) -> FiniteSetUpdate p -> FiniteSetUpdate q
    mapFiniteSetUpdate _ (KeyUpdateItem _ update) = never update
    mapFiniteSetUpdate pq (KeyUpdateDelete p) = KeyUpdateDelete $ pq p
    mapFiniteSetUpdate pq (KeyUpdateInsertReplace p) = KeyUpdateInsertReplace $ pq p
    mapFiniteSetUpdate _ KeyUpdateClear = KeyUpdateClear
    mapFiniteSetEdit :: forall p q. (p -> q) -> FiniteSetEdit p -> FiniteSetEdit q
    mapFiniteSetEdit _ (KeyEditItem _ edit) = never edit
    mapFiniteSetEdit pq (KeyEditDelete p) = KeyEditDelete $ pq p
    mapFiniteSetEdit pq (KeyEditInsertReplace p) = KeyEditInsertReplace $ pq p
    mapFiniteSetEdit _ KeyEditClear = KeyEditClear
    clRead :: ReadFunction (FiniteSetReader a) (FiniteSetReader b)
    clRead mra KeyReadKeys = fmap (biIsoMap aba) $ mra KeyReadKeys
    clRead mra (KeyReadItem b ReadWhole) = fmap (fmap ab) $ mra $ KeyReadItem (ba b) ReadWhole
    clUpdate ::
        forall m.
        MonadIO m =>
        FiniteSetUpdate a ->
        Readable m (FiniteSetReader a) ->
        m [FiniteSetUpdate b]
    clUpdate ea _ = return $ pure $ mapFiniteSetUpdate ab ea
    clPutEdits ::
        forall m.
        MonadIO m =>
        [FiniteSetEdit b] ->
        Readable m (FiniteSetReader a) ->
        m (Maybe [FiniteSetEdit a])
    clPutEdits ebs _ = return $ Just $ fmap (mapFiniteSetEdit ba) ebs
    in MkChangeLens{..}

finiteSetCartesianSumChangeLens ::
    forall a b. ChangeLens (PairUpdate (FiniteSetUpdate a) (FiniteSetUpdate b)) (FiniteSetUpdate (Either a b))
finiteSetCartesianSumChangeLens = let
    clRead :: ReadFunction (PairUpdateReader (FiniteSetUpdate a) (FiniteSetUpdate b)) (FiniteSetReader (Either a b))
    clRead mr KeyReadKeys = do
        aa <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
        bb <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
        return $ aa <+++> bb
    clRead mr (KeyReadItem (Left a) ReadWhole) = do
        ma' <- mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem a ReadWhole
        return $ fmap Left ma'
    clRead mr (KeyReadItem (Right b) ReadWhole) = do
        mb' <- mr $ MkTupleUpdateReader SelectSecond $ KeyReadItem b ReadWhole
        return $ fmap Right mb'
    clUpdate ::
        forall m.
        MonadIO m =>
        PairUpdate (FiniteSetUpdate a) (FiniteSetUpdate b) ->
        Readable m (PairUpdateReader (FiniteSetUpdate a) (FiniteSetUpdate b)) ->
        m [FiniteSetUpdate (Either a b)]
    clUpdate (MkTupleUpdate SelectFirst (KeyUpdateItem _ edit)) _ = never edit
    clUpdate (MkTupleUpdate SelectFirst (KeyUpdateDelete v)) _ = return $ pure $ KeyUpdateDelete $ Left v
    clUpdate (MkTupleUpdate SelectFirst (KeyUpdateInsertReplace v)) _ = return $ pure $ KeyUpdateInsertReplace $ Left v
    clUpdate (MkTupleUpdate SelectFirst KeyUpdateClear) mr = do
        vv <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
        for (toList vv) $ \v -> return $ KeyUpdateDelete $ Left v
    clUpdate (MkTupleUpdate SelectSecond (KeyUpdateItem _ edit)) _ = never edit
    clUpdate (MkTupleUpdate SelectSecond (KeyUpdateDelete v)) _ = return $ pure $ KeyUpdateDelete $ Right v
    clUpdate (MkTupleUpdate SelectSecond (KeyUpdateInsertReplace v)) _ =
        return $ pure $ KeyUpdateInsertReplace $ Right v
    clUpdate (MkTupleUpdate SelectSecond KeyUpdateClear) mr = do
        vv <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
        for (toList vv) $ \v -> return $ KeyUpdateDelete $ Right v
    clPutEdits ::
        forall m.
        MonadIO m =>
        [FiniteSetEdit (Either a b)] ->
        Readable m (PairUpdateReader (FiniteSetUpdate a) (FiniteSetUpdate b)) ->
        m (Maybe [PairUpdateEdit (FiniteSetUpdate a) (FiniteSetUpdate b)])
    clPutEdits =
        clPutEditsFromSimplePutEdit $ \case
            KeyEditItem _ e -> never e
            KeyEditDelete (Left v) -> return $ Just $ pure $ MkTupleUpdateEdit SelectFirst $ KeyEditDelete v
            KeyEditDelete (Right v) -> return $ Just $ pure $ MkTupleUpdateEdit SelectSecond $ KeyEditDelete v
            KeyEditInsertReplace (Left v) ->
                return $ Just $ pure $ MkTupleUpdateEdit SelectFirst $ KeyEditInsertReplace v
            KeyEditInsertReplace (Right v) ->
                return $ Just $ pure $ MkTupleUpdateEdit SelectSecond $ KeyEditInsertReplace v
            KeyEditClear ->
                return
                    $ Just
                    $ [MkTupleUpdateEdit SelectFirst KeyEditClear, MkTupleUpdateEdit SelectSecond KeyEditClear]
    in MkChangeLens{..}

finiteSetCartesianProductUpdateFunction ::
    forall a b.
    ChangeLens (PairUpdate (FiniteSetUpdate a) (FiniteSetUpdate b)) (ReadOnlyUpdate (FiniteSetUpdate (a, b)))
finiteSetCartesianProductUpdateFunction = let
    clRead :: ReadFunction (PairUpdateReader (FiniteSetUpdate a) (FiniteSetUpdate b)) (FiniteSetReader (a, b))
    clRead mr KeyReadKeys = do
        aa <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
        bb <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
        return $ aa <***> bb
    clRead mr (KeyReadItem (a, b) ReadWhole) =
        unComposeInner $ do
            a' <- MkComposeInner $ mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem a ReadWhole
            b' <- MkComposeInner $ mr $ MkTupleUpdateReader SelectSecond $ KeyReadItem b ReadWhole
            return (a', b')
    clUpdate ::
        forall m.
        MonadIO m =>
        PairUpdate (FiniteSetUpdate a) (FiniteSetUpdate b) ->
        Readable m (PairUpdateReader (FiniteSetUpdate a) (FiniteSetUpdate b)) ->
        m [ReadOnlyUpdate (FiniteSetUpdate (a, b))]
    clUpdate (MkTupleUpdate SelectFirst KeyUpdateClear) _ = return [MkReadOnlyUpdate KeyUpdateClear]
    clUpdate (MkTupleUpdate SelectSecond KeyUpdateClear) _ = return [MkReadOnlyUpdate KeyUpdateClear]
    clUpdate (MkTupleUpdate SelectFirst (KeyUpdateItem _ update)) _ = never update
    clUpdate (MkTupleUpdate SelectSecond (KeyUpdateItem _ update)) _ = never update
    clUpdate (MkTupleUpdate SelectFirst (KeyUpdateDelete a)) mr = do
        bb <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
        return $ fmap (\b -> MkReadOnlyUpdate $ KeyUpdateDelete (a, b)) $ toList bb
    clUpdate (MkTupleUpdate SelectSecond (KeyUpdateDelete b)) mr = do
        aa <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
        return $ fmap (\a -> MkReadOnlyUpdate $ KeyUpdateDelete (a, b)) $ toList aa
    clUpdate (MkTupleUpdate SelectFirst (KeyUpdateInsertReplace a)) mr = do
        bb <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
        return $ fmap (\b -> MkReadOnlyUpdate $ KeyUpdateInsertReplace (a, b)) $ toList bb
    clUpdate (MkTupleUpdate SelectSecond (KeyUpdateInsertReplace b)) mr = do
        aa <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
        return $ fmap (\a -> MkReadOnlyUpdate $ KeyUpdateInsertReplace (a, b)) $ toList aa
    in MkChangeLens{clPutEdits = clPutEditsNone, ..}

finiteSetFunctionChangeLens :: forall a. ChangeLens (FiniteSetUpdate a) (PartialSetUpdate a)
finiteSetFunctionChangeLens = let
    clRead :: ReadFunction (FiniteSetReader a) (FunctionUpdateReader a (WholeUpdate Bool))
    clRead mr (MkTupleUpdateReader (MkFunctionSelector a) ReadWhole) = do
        mu <- mr $ KeyReadItem a ReadWhole
        return $ isJust mu
    clUpdate ::
        forall m.
        MonadIO m =>
        FiniteSetUpdate a ->
        Readable m (FiniteSetReader a) ->
        m [PartialUpdate (FunctionUpdate a (WholeUpdate Bool))]
    clUpdate (KeyUpdateItem _ update) _ = never update
    clUpdate (KeyUpdateDelete p) _ =
        return [KnownPartialUpdate $ MkTupleUpdate (MkFunctionSelector p) (MkWholeReaderUpdate False)]
    clUpdate (KeyUpdateInsertReplace p) _ =
        return [KnownPartialUpdate $ MkTupleUpdate (MkFunctionSelector p) (MkWholeReaderUpdate True)]
    clUpdate KeyUpdateClear _ = return [UnknownPartialUpdate $ \_ -> True]
    clPutEdit ::
        forall m.
        MonadIO m =>
        FunctionUpdateEdit a (WholeUpdate Bool) ->
        m (Maybe [FiniteSetEdit a])
    clPutEdit (MkTupleUpdateEdit (MkFunctionSelector a) (MkWholeReaderEdit b)) =
        return
            $ Just
                [ if b
                    then KeyEditInsertReplace a
                    else KeyEditDelete a
                ]
    clPutEdits ::
        forall m.
        MonadIO m =>
        [FunctionUpdateEdit a (WholeUpdate Bool)] ->
        Readable m (FiniteSetReader a) ->
        m (Maybe [FiniteSetEdit a])
    clPutEdits = clPutEditsFromSimplePutEdit clPutEdit
    in MkChangeLens{..}

filterFiniteSetUpdateFunction ::
    forall a. ChangeLens (PairUpdate (FiniteSetUpdate a) (PartialSetUpdate a)) (ReadOnlyUpdate (FiniteSetUpdate a))
filterFiniteSetUpdateFunction = let
    getFSReplaceEdits ::
        forall m.
        MonadIO m =>
        Readable m (FiniteSetReader a) ->
        m [FiniteSetEdit a]
    getFSReplaceEdits mr = do
        fs <- mr KeyReadKeys
        return $ KeyEditClear : fmap KeyEditInsertReplace (toList fs)
    maybeItem1 :: forall m. Readable m (PairUpdateReader (FiniteSetUpdate a) (PartialSetUpdate a)) -> a -> m (Maybe a)
    maybeItem1 mr a = mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem a ReadWhole
    testItem2 :: forall m. Readable m (PairUpdateReader (FiniteSetUpdate a) (PartialSetUpdate a)) -> a -> m Bool
    testItem2 mr a = mr $ MkTupleUpdateReader SelectSecond $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole
    maybeItem2 ::
        forall m.
        MonadIO m =>
        Readable m (PairUpdateReader (FiniteSetUpdate a) (PartialSetUpdate a)) ->
        a ->
        m (Maybe a)
    maybeItem2 mr a = do
        x <- testItem2 mr a
        return
            $ if x
                then Just a
                else Nothing
    clRead ::
        forall m.
        MonadIO m =>
        Readable m (PairUpdateReader (FiniteSetUpdate a) (PartialSetUpdate a)) ->
        Readable m (FiniteSetReader a)
    clRead mr (KeyReadItem a ReadWhole) = do
        mu <- maybeItem1 mr a
        case mu of
            Nothing -> return Nothing
            Just _ -> maybeItem2 mr a
    clRead mr KeyReadKeys = do
        aa <- mr $ MkTupleUpdateReader SelectFirst $ KeyReadKeys
        ofilterM (testItem2 mr) aa
    clUpdate ::
        forall m.
        MonadIO m =>
        PairUpdate (FiniteSetUpdate a) (PartialSetUpdate a) ->
        Readable m (PairUpdateReader (FiniteSetUpdate a) (PartialSetUpdate a)) ->
        m [ReadOnlyUpdate (FiniteSetUpdate a)]
    clUpdate (MkTupleUpdate SelectFirst (KeyUpdateItem _ update)) _mr = never update
    clUpdate (MkTupleUpdate SelectFirst KeyUpdateClear) _mr = return $ pure $ MkReadOnlyUpdate KeyUpdateClear
    clUpdate (MkTupleUpdate SelectFirst (KeyUpdateInsertReplace a)) mr = do
        mu <- maybeItem2 mr a
        return
            $ case mu of
                Nothing -> []
                Just _ -> [MkReadOnlyUpdate $ KeyUpdateInsertReplace a]
    clUpdate (MkTupleUpdate SelectFirst (KeyUpdateDelete a)) mr = do
        mu <- maybeItem2 mr a
        return
            $ case mu of
                Nothing -> []
                Just _ -> [MkReadOnlyUpdate $ KeyUpdateDelete a]
    clUpdate (MkTupleUpdate SelectSecond (KnownPartialUpdate (MkTupleUpdate (MkFunctionSelector a) (MkWholeReaderUpdate t)))) mr = do
        mu <- maybeItem1 mr a
        return
            $ pure
            $ MkReadOnlyUpdate
            $ case mu of
                Nothing -> KeyUpdateDelete a
                Just _ ->
                    if t
                        then KeyUpdateInsertReplace a
                        else KeyUpdateDelete a
    clUpdate (MkTupleUpdate SelectSecond (UnknownPartialUpdate _)) mr = do
        edits <- getFSReplaceEdits $ firstReadFunction mr
        return $ fmap (MkReadOnlyUpdate . editUpdate) edits
    in MkChangeLens{clPutEdits = clPutEditsNone, ..}
