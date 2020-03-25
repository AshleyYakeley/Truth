{-# OPTIONS -fno-warn-orphans #-}

module Truth.Core.Types.Key.FiniteSet where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Lens
import Truth.Core.Read
import Truth.Core.Types.Key.Key
import Truth.Core.Types.Lattice
import Truth.Core.Types.None
import Truth.Core.Types.Partial
import Truth.Core.Types.ReadOnly
import Truth.Core.Types.Set
import Truth.Core.Types.Tuple.Function
import Truth.Core.Types.Tuple.Pair
import Truth.Core.Types.Tuple.Tuple
import Truth.Core.Types.Unit
import Truth.Core.Types.Whole

{- equivalent to:
data FiniteSetReader subj t where
    KeyReadKeys :: FiniteSetReader subj (FiniteSet subj)
    KeyReadItem :: subj -> WholeReader subj t -> FiniteSetReader subj (Maybe t)
-}
type FiniteSetReader subj = KeyReader (FiniteSet subj) (WholeReader subj)

{- equivalent to:
data FiniteSetEdit subj where
    KeyEditItem :: subj -> ConstWholeEdit subj -> FiniteSetEdit subj
    KeyEditDelete :: subj -> FiniteSetEdit subj
    KeyEditInsertReplace :: subj -> FiniteSetEdit subj
    KeyEditClear :: FiniteSetEdit subj
-}
type FiniteSetEdit subj = KeyEdit (FiniteSet subj) (ConstWholeEdit subj)

{- equivalent to:
data FiniteSetUpdate subj where
    KeyUpdateItem :: subj -> ConstWholeUpdate subj -> FiniteSetUpdate subj
    KeyUpdateDelete :: subj -> FiniteSetUpdate subj
    KeyUpdateInsertReplace :: subj -> FiniteSetUpdate subj
    KeyUpdateClear :: FiniteSetUpdate subj
-}
type FiniteSetUpdate subj = KeyUpdate (FiniteSet subj) (ConstWholeUpdate subj)

wholeFiniteSetReadFunction :: Eq subj => ReadFunction (WholeReader (FiniteSet subj)) (FiniteSetReader subj)
wholeFiniteSetReadFunction mr KeyReadKeys = mr ReadWhole
wholeFiniteSetReadFunction mr (KeyReadItem subj ReadWhole) = do
    fset <- mr ReadWhole
    return $
        if member subj fset
            then Just subj
            else Nothing

finiteSetChangeLens ::
       forall subj. Eq subj
    => subj
    -> ChangeLens (FiniteSetUpdate subj) (WholeUpdate Bool)
finiteSetChangeLens subj = let
    clRead :: ReadFunction (FiniteSetReader subj) (WholeReader Bool)
    clRead mr ReadWhole = fmap isJust $ mr $ KeyReadItem subj ReadWhole
    clUpdate ::
           forall m. MonadIO m
        => FiniteSetUpdate subj
        -> Readable m (FiniteSetReader subj)
        -> m [WholeUpdate Bool]
    clUpdate (KeyUpdateItem _ update) _ = never update
    clUpdate (KeyUpdateDelete key) _
        | key == subj = return [MkWholeReaderUpdate False]
    clUpdate (KeyUpdateDelete _) _ = return []
    clUpdate (KeyUpdateInsertReplace key) _
        | key == subj = return [MkWholeReaderUpdate True]
    clUpdate (KeyUpdateInsertReplace _) _ = return []
    clUpdate KeyUpdateClear _ = return [MkWholeReaderUpdate False]
    elPutEdit ::
           forall m. MonadIO m
        => WholeEdit Bool
        -> m (Maybe [FiniteSetEdit subj])
    elPutEdit (MkWholeReaderEdit False) = return $ Just [KeyEditDelete subj]
    elPutEdit (MkWholeReaderEdit True) = return $ Just [KeyEditInsertReplace subj]
    clPutEdits ::
           forall m. MonadIO m
        => [WholeEdit Bool]
        -> Readable m (FiniteSetReader subj)
        -> m (Maybe [FiniteSetEdit subj])
    clPutEdits = clPutEditsFromSimplePutEdit elPutEdit
    in MkChangeLens {..}

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
            return $
                if r1 \/ r2
                    then Just item
                    else Nothing
        clUpdate ::
               forall m. MonadIO m
            => PairUpdate (FiniteSetUpdate subj) (FiniteSetUpdate subj)
            -> Readable m (PairUpdateReader (FiniteSetUpdate subj) (FiniteSetUpdate subj))
            -> m [ReadOnlyUpdate (FiniteSetUpdate subj)]
        clUpdate (MkTupleUpdate SelectFirst (KeyUpdateItem _ edit)) _ = never edit
        clUpdate (MkTupleUpdate SelectFirst (KeyUpdateDelete item)) mr = do
            (isJust -> r2) <- mr $ MkTupleUpdateReader SelectSecond $ KeyReadItem item ReadWhole
            return $
                if r2
                    then []
                    else [MkReadOnlyUpdate $ KeyUpdateDelete item]
        clUpdate (MkTupleUpdate SelectFirst (KeyUpdateInsertReplace item)) mr = do
            (isJust -> r2) <- mr $ MkTupleUpdateReader SelectSecond $ KeyReadItem item ReadWhole
            return $
                if r2
                    then []
                    else [MkReadOnlyUpdate $ KeyUpdateInsertReplace item]
        clUpdate (MkTupleUpdate SelectFirst KeyUpdateClear) mr = do
            keys1 <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
            keys2 <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
            return $
                case (null keys1, null keys2) of
                    (True, _) -> []
                    (False, True) -> [MkReadOnlyUpdate KeyUpdateClear]
                    (False, False) -> fmap (MkReadOnlyUpdate . KeyUpdateDelete) $ toList $ difference keys1 keys2
        clUpdate (MkTupleUpdate SelectSecond (KeyUpdateItem _ edit)) _ = never edit
        clUpdate (MkTupleUpdate SelectSecond (KeyUpdateDelete item)) mr = do
            (isJust -> r1) <- mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem item ReadWhole
            return $
                if r1
                    then []
                    else [MkReadOnlyUpdate $ KeyUpdateDelete item]
        clUpdate (MkTupleUpdate SelectSecond (KeyUpdateInsertReplace item)) mr = do
            (isJust -> r1) <- mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem item ReadWhole
            return $
                if r1
                    then []
                    else [MkReadOnlyUpdate $ KeyUpdateInsertReplace item]
        clUpdate (MkTupleUpdate SelectSecond KeyUpdateClear) mr = do
            keys2 <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
            keys1 <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
            return $
                case (null keys2, null keys1) of
                    (True, _) -> []
                    (False, True) -> [MkReadOnlyUpdate KeyUpdateClear]
                    (False, False) -> fmap (MkReadOnlyUpdate . KeyUpdateDelete) $ toList $ difference keys2 keys1
        in MkChangeLens {clPutEdits = clPutEditsNone, ..}

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
            return $
                if r1 /\ r2
                    then Just item
                    else Nothing
        clUpdate ::
               forall m. MonadIO m
            => PairUpdate (FiniteSetUpdate subj) (FiniteSetUpdate subj)
            -> Readable m (PairUpdateReader (FiniteSetUpdate subj) (FiniteSetUpdate subj))
            -> m [ReadOnlyUpdate (FiniteSetUpdate subj)]
        clUpdate (MkTupleUpdate SelectFirst (KeyUpdateItem _ edit)) _ = never edit
        clUpdate (MkTupleUpdate SelectFirst (KeyUpdateDelete item)) mr = do
            (isJust -> r2) <- mr $ MkTupleUpdateReader SelectSecond $ KeyReadItem item ReadWhole
            return $
                if r2
                    then [MkReadOnlyUpdate $ KeyUpdateDelete item]
                    else []
        clUpdate (MkTupleUpdate SelectFirst (KeyUpdateInsertReplace item)) mr = do
            (isJust -> r2) <- mr $ MkTupleUpdateReader SelectSecond $ KeyReadItem item ReadWhole
            return $
                if r2
                    then [MkReadOnlyUpdate $ KeyUpdateInsertReplace item]
                    else []
        clUpdate (MkTupleUpdate SelectFirst KeyUpdateClear) mr = do
            keys1 <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
            keys2 <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
            return $
                if null $ keys1 /\ keys2
                    then []
                    else [MkReadOnlyUpdate KeyUpdateClear]
        clUpdate (MkTupleUpdate SelectSecond (KeyUpdateItem _ edit)) _ = never edit
        clUpdate (MkTupleUpdate SelectSecond (KeyUpdateDelete item)) mr = do
            (isJust -> r1) <- mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem item ReadWhole
            return $
                if r1
                    then [MkReadOnlyUpdate $ KeyUpdateDelete item]
                    else []
        clUpdate (MkTupleUpdate SelectSecond (KeyUpdateInsertReplace item)) mr = do
            (isJust -> r1) <- mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem item ReadWhole
            return $
                if r1
                    then [MkReadOnlyUpdate $ KeyUpdateInsertReplace item]
                    else []
        clUpdate (MkTupleUpdate SelectSecond KeyUpdateClear) mr = do
            keys2 <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
            keys1 <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
            return $
                if null $ keys2 /\ keys1
                    then []
                    else [MkReadOnlyUpdate KeyUpdateClear]
        in MkChangeLens {clPutEdits = clPutEditsNone, ..}

bijectionFiniteSetChangeLens :: forall a b. Bijection a b -> ChangeLens (FiniteSetUpdate a) (FiniteSetUpdate b)
bijectionFiniteSetChangeLens (MkIsomorphism ab ba) = let
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
    clRead ::
           forall m t. MonadIO m
        => Readable m (FiniteSetReader a)
        -> FiniteSetReader b t
        -> m t
    clRead mra KeyReadKeys = fmap (fmap ab) $ mra KeyReadKeys
    clRead mra (KeyReadItem b ReadWhole) = fmap (fmap ab) $ mra $ KeyReadItem (ba b) ReadWhole
    clUpdate ::
           forall m. MonadIO m
        => FiniteSetUpdate a
        -> Readable m (FiniteSetReader a)
        -> m [FiniteSetUpdate b]
    clUpdate ea _ = return $ pure $ mapFiniteSetUpdate ab ea
    clPutEdits ::
           forall m. MonadIO m
        => [FiniteSetEdit b]
        -> Readable m (FiniteSetReader a)
        -> m (Maybe [FiniteSetEdit a])
    clPutEdits ebs _ = return $ Just $ fmap (mapFiniteSetEdit ba) ebs
    in MkChangeLens {..}

finiteSetCartesianSumChangeLens ::
       forall a b. (Eq a, Eq b)
    => ChangeLens (PairUpdate (FiniteSetUpdate a) (FiniteSetUpdate b)) (FiniteSetUpdate (Either a b))
finiteSetCartesianSumChangeLens = let
    clRead :: ReadFunction (PairUpdateReader (FiniteSetUpdate a) (FiniteSetUpdate b)) (FiniteSetReader (Either a b))
    clRead mr KeyReadKeys = do
        aa <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
        bb <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
        return $ fmap Left aa <> fmap Right bb
    clRead mr (KeyReadItem (Left a) ReadWhole) = do
        ma' <- mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem a ReadWhole
        return $ fmap Left ma'
    clRead mr (KeyReadItem (Right b) ReadWhole) = do
        mb' <- mr $ MkTupleUpdateReader SelectSecond $ KeyReadItem b ReadWhole
        return $ fmap Right mb'
    clUpdate ::
           forall m. MonadIO m
        => PairUpdate (FiniteSetUpdate a) (FiniteSetUpdate b)
        -> Readable m (PairUpdateReader (FiniteSetUpdate a) (FiniteSetUpdate b))
        -> m [FiniteSetUpdate (Either a b)]
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
           forall m. MonadIO m
        => [FiniteSetEdit (Either a b)]
        -> Readable m (PairUpdateReader (FiniteSetUpdate a) (FiniteSetUpdate b))
        -> m (Maybe [PairUpdateEdit (FiniteSetUpdate a) (FiniteSetUpdate b)])
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
                return $
                Just $ [MkTupleUpdateEdit SelectFirst KeyEditClear, MkTupleUpdateEdit SelectSecond KeyEditClear]
    in MkChangeLens {..}

finiteSetCartesianProductUpdateFunction ::
       forall a b.
       ChangeLens (PairUpdate (FiniteSetUpdate a) (FiniteSetUpdate b)) (ReadOnlyUpdate (FiniteSetUpdate (a, b)))
finiteSetCartesianProductUpdateFunction = let
    clRead :: ReadFunction (PairUpdateReader (FiniteSetUpdate a) (FiniteSetUpdate b)) (FiniteSetReader (a, b))
    clRead mr KeyReadKeys = do
        aa <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
        bb <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
        return $ liftA2 (,) aa bb
    clRead mr (KeyReadItem (a, b) ReadWhole) =
        getComposeM $ do
            a' <- MkComposeM $ mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem a ReadWhole
            b' <- MkComposeM $ mr $ MkTupleUpdateReader SelectSecond $ KeyReadItem b ReadWhole
            return (a', b')
    clUpdate ::
           forall m. MonadIO m
        => PairUpdate (FiniteSetUpdate a) (FiniteSetUpdate b)
        -> Readable m (PairUpdateReader (FiniteSetUpdate a) (FiniteSetUpdate b))
        -> m [ReadOnlyUpdate (FiniteSetUpdate (a, b))]
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
    in MkChangeLens {clPutEdits = clPutEditsNone, ..}

finiteSetFunctionChangeLens :: forall a. ChangeLens (FiniteSetUpdate a) (PartialSetUpdate a)
finiteSetFunctionChangeLens = let
    clRead ::
           forall m t. MonadIO m
        => Readable m (FiniteSetReader a)
        -> FunctionUpdateReader a (WholeUpdate Bool) t
        -> m t
    clRead mr (MkTupleUpdateReader (MkFunctionSelector a) ReadWhole) = do
        mu <- mr $ KeyReadItem a ReadWhole
        return $ isJust mu
    clUpdate ::
           forall m. MonadIO m
        => FiniteSetUpdate a
        -> Readable m (FiniteSetReader a)
        -> m [PartialUpdate (FunctionUpdate a (WholeUpdate Bool))]
    clUpdate (KeyUpdateItem _ update) _ = never update
    clUpdate (KeyUpdateDelete p) _ =
        return [KnownPartialUpdate $ MkTupleUpdate (MkFunctionSelector p) (MkWholeReaderUpdate False)]
    clUpdate (KeyUpdateInsertReplace p) _ =
        return [KnownPartialUpdate $ MkTupleUpdate (MkFunctionSelector p) (MkWholeReaderUpdate True)]
    clUpdate KeyUpdateClear _ = return [UnknownPartialUpdate $ \_ -> True]
    elPutEdit ::
           forall m. MonadIO m
        => FunctionUpdateEdit a (WholeUpdate Bool)
        -> m (Maybe [FiniteSetEdit a])
    elPutEdit (MkTupleUpdateEdit (MkFunctionSelector a) (MkWholeReaderEdit b)) =
        return $
        Just
            [ if b
                  then KeyEditInsertReplace a
                  else KeyEditDelete a
            ]
    clPutEdits ::
           forall m. MonadIO m
        => [FunctionUpdateEdit a (WholeUpdate Bool)]
        -> Readable m (FiniteSetReader a)
        -> m (Maybe [FiniteSetEdit a])
    clPutEdits = clPutEditsFromSimplePutEdit elPutEdit
    in MkChangeLens {..}

filterFiniteSetUpdateFunction ::
       forall a. Eq a
    => ChangeLens (PairUpdate (FiniteSetUpdate a) (PartialSetUpdate a)) (ReadOnlyUpdate (FiniteSetUpdate a))
filterFiniteSetUpdateFunction = let
    testItem1 ::
           forall m. MonadIO m
        => Readable m (PairUpdateReader (FiniteSetUpdate a) (PartialSetUpdate a))
        -> a
        -> m (Maybe a)
    testItem1 mr a = mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem a ReadWhole
    testItem2 ::
           forall m. MonadIO m
        => Readable m (PairUpdateReader (FiniteSetUpdate a) (PartialSetUpdate a))
        -> a
        -> m (Maybe a)
    testItem2 mr a = do
        x <- mr $ MkTupleUpdateReader SelectSecond $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole
        return $
            if x
                then Just a
                else Nothing
    clRead ::
           forall m. MonadIO m
        => Readable m (PairUpdateReader (FiniteSetUpdate a) (PartialSetUpdate a))
        -> Readable m (FiniteSetReader a)
    clRead mr (KeyReadItem a ReadWhole) = do
        mu <- testItem1 mr a
        case mu of
            Nothing -> return Nothing
            Just _ -> testItem2 mr a
    clRead mr KeyReadKeys = do
        aa <- mr $ MkTupleUpdateReader SelectFirst $ KeyReadKeys
        aa' <- for aa $ testItem2 mr
        return $ catMaybes aa'
    clUpdate ::
           forall m. MonadIO m
        => PairUpdate (FiniteSetUpdate a) (PartialSetUpdate a)
        -> Readable m (PairUpdateReader (FiniteSetUpdate a) (PartialSetUpdate a))
        -> m [ReadOnlyUpdate (FiniteSetUpdate a)]
    clUpdate (MkTupleUpdate SelectFirst (KeyUpdateItem _ update)) _mr = never update
    clUpdate (MkTupleUpdate SelectFirst KeyUpdateClear) _mr = return $ pure $ MkReadOnlyUpdate KeyUpdateClear
    clUpdate (MkTupleUpdate SelectFirst (KeyUpdateInsertReplace a)) mr = do
        mu <- testItem2 mr a
        return $
            case mu of
                Nothing -> []
                Just _ -> [MkReadOnlyUpdate $ KeyUpdateInsertReplace a]
    clUpdate (MkTupleUpdate SelectFirst (KeyUpdateDelete a)) mr = do
        mu <- testItem2 mr a
        return $
            case mu of
                Nothing -> []
                Just _ -> [MkReadOnlyUpdate $ KeyUpdateDelete a]
    clUpdate (MkTupleUpdate SelectSecond (KnownPartialUpdate (MkTupleUpdate (MkFunctionSelector a) (MkWholeReaderUpdate t)))) mr = do
        mu <- testItem1 mr a
        return $
            pure $
            MkReadOnlyUpdate $
            case mu of
                Nothing -> KeyUpdateDelete a
                Just _ ->
                    if t
                        then KeyUpdateInsertReplace a
                        else KeyUpdateDelete a
    clUpdate (MkTupleUpdate SelectSecond (UnknownPartialUpdate _)) mr = do
        edits <- getReplaceEdits $ firstReadFunction mr
        return $ fmap (MkReadOnlyUpdate . editUpdate) edits
    in MkChangeLens {clPutEdits = clPutEditsNone, ..}
