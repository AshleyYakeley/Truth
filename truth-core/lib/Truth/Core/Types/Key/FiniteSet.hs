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

finiteSetEditLens ::
       forall subj. Eq subj
    => subj
    -> EditLens (FiniteSetUpdate subj) (WholeUpdate Bool)
finiteSetEditLens subj = let
    elGet :: ReadFunction (FiniteSetReader subj) (WholeReader Bool)
    elGet mr ReadWhole = fmap isJust $ mr $ KeyReadItem subj ReadWhole
    elUpdate ::
           forall m. MonadIO m
        => FiniteSetUpdate subj
        -> MutableRead m (FiniteSetReader subj)
        -> m [WholeUpdate Bool]
    elUpdate (KeyUpdateItem _ update) _ = never update
    elUpdate (KeyUpdateDelete key) _
        | key == subj = return [MkWholeReaderUpdate False]
    elUpdate (KeyUpdateDelete _) _ = return []
    elUpdate (KeyUpdateInsertReplace key) _
        | key == subj = return [MkWholeReaderUpdate True]
    elUpdate (KeyUpdateInsertReplace _) _ = return []
    elUpdate KeyUpdateClear _ = return [MkWholeReaderUpdate False]
    elPutEdit ::
           forall m. MonadIO m
        => WholeEdit Bool
        -> m (Maybe [FiniteSetEdit subj])
    elPutEdit (MkWholeReaderEdit False) = return $ Just [KeyEditDelete subj]
    elPutEdit (MkWholeReaderEdit True) = return $ Just [KeyEditInsertReplace subj]
    elPutEdits ::
           forall m. MonadIO m
        => [WholeEdit Bool]
        -> MutableRead m (FiniteSetReader subj)
        -> m (Maybe [FiniteSetEdit subj])
    elPutEdits = elPutEditsFromSimplePutEdit elPutEdit
    in MkEditLens {..}

instance Eq subj => JoinSemiLatticeReadOnlyEditLens (FiniteSetUpdate subj) where
    joinEditLens = let
        elGet :: ReadFunction (PairUpdateReader (FiniteSetUpdate subj) (FiniteSetUpdate subj)) (FiniteSetReader subj)
        elGet mr KeyReadKeys = do
            keys1 <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
            keys2 <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
            return $ keys1 \/ keys2
        elGet mr (KeyReadItem item ReadWhole) = do
            (isJust -> r1) <- mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem item ReadWhole
            (isJust -> r2) <- mr $ MkTupleUpdateReader SelectSecond $ KeyReadItem item ReadWhole
            return $
                if r1 \/ r2
                    then Just item
                    else Nothing
        elUpdate ::
               forall m. MonadIO m
            => PairUpdate (FiniteSetUpdate subj) (FiniteSetUpdate subj)
            -> MutableRead m (PairUpdateReader (FiniteSetUpdate subj) (FiniteSetUpdate subj))
            -> m [ReadOnlyUpdate (FiniteSetUpdate subj)]
        elUpdate (MkTupleUpdate SelectFirst (KeyUpdateItem _ edit)) _ = never edit
        elUpdate (MkTupleUpdate SelectFirst (KeyUpdateDelete item)) mr = do
            (isJust -> r2) <- mr $ MkTupleUpdateReader SelectSecond $ KeyReadItem item ReadWhole
            return $
                if r2
                    then []
                    else [MkReadOnlyUpdate $ KeyUpdateDelete item]
        elUpdate (MkTupleUpdate SelectFirst (KeyUpdateInsertReplace item)) mr = do
            (isJust -> r2) <- mr $ MkTupleUpdateReader SelectSecond $ KeyReadItem item ReadWhole
            return $
                if r2
                    then []
                    else [MkReadOnlyUpdate $ KeyUpdateInsertReplace item]
        elUpdate (MkTupleUpdate SelectFirst KeyUpdateClear) mr = do
            keys1 <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
            keys2 <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
            return $
                case (null keys1, null keys2) of
                    (True, _) -> []
                    (False, True) -> [MkReadOnlyUpdate KeyUpdateClear]
                    (False, False) -> fmap (MkReadOnlyUpdate . KeyUpdateDelete) $ toList $ difference keys1 keys2
        elUpdate (MkTupleUpdate SelectSecond (KeyUpdateItem _ edit)) _ = never edit
        elUpdate (MkTupleUpdate SelectSecond (KeyUpdateDelete item)) mr = do
            (isJust -> r1) <- mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem item ReadWhole
            return $
                if r1
                    then []
                    else [MkReadOnlyUpdate $ KeyUpdateDelete item]
        elUpdate (MkTupleUpdate SelectSecond (KeyUpdateInsertReplace item)) mr = do
            (isJust -> r1) <- mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem item ReadWhole
            return $
                if r1
                    then []
                    else [MkReadOnlyUpdate $ KeyUpdateInsertReplace item]
        elUpdate (MkTupleUpdate SelectSecond KeyUpdateClear) mr = do
            keys2 <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
            keys1 <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
            return $
                case (null keys2, null keys1) of
                    (True, _) -> []
                    (False, True) -> [MkReadOnlyUpdate KeyUpdateClear]
                    (False, False) -> fmap (MkReadOnlyUpdate . KeyUpdateDelete) $ toList $ difference keys2 keys1
        in MkEditLens {elPutEdits = elPutEditsNone, ..}

instance Eq subj => MeetSemiLatticeReadOnlyEditLens (FiniteSetUpdate subj) where
    meetEditLens = let
        elGet :: ReadFunction (PairUpdateReader (FiniteSetUpdate subj) (FiniteSetUpdate subj)) (FiniteSetReader subj)
        elGet mr KeyReadKeys = do
            keys1 <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
            keys2 <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
            return $ keys1 /\ keys2
        elGet mr (KeyReadItem item ReadWhole) = do
            (isJust -> r1) <- mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem item ReadWhole
            (isJust -> r2) <- mr $ MkTupleUpdateReader SelectSecond $ KeyReadItem item ReadWhole
            return $
                if r1 /\ r2
                    then Just item
                    else Nothing
        elUpdate ::
               forall m. MonadIO m
            => PairUpdate (FiniteSetUpdate subj) (FiniteSetUpdate subj)
            -> MutableRead m (PairUpdateReader (FiniteSetUpdate subj) (FiniteSetUpdate subj))
            -> m [ReadOnlyUpdate (FiniteSetUpdate subj)]
        elUpdate (MkTupleUpdate SelectFirst (KeyUpdateItem _ edit)) _ = never edit
        elUpdate (MkTupleUpdate SelectFirst (KeyUpdateDelete item)) mr = do
            (isJust -> r2) <- mr $ MkTupleUpdateReader SelectSecond $ KeyReadItem item ReadWhole
            return $
                if r2
                    then [MkReadOnlyUpdate $ KeyUpdateDelete item]
                    else []
        elUpdate (MkTupleUpdate SelectFirst (KeyUpdateInsertReplace item)) mr = do
            (isJust -> r2) <- mr $ MkTupleUpdateReader SelectSecond $ KeyReadItem item ReadWhole
            return $
                if r2
                    then [MkReadOnlyUpdate $ KeyUpdateInsertReplace item]
                    else []
        elUpdate (MkTupleUpdate SelectFirst KeyUpdateClear) mr = do
            keys1 <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
            keys2 <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
            return $
                if null $ keys1 /\ keys2
                    then []
                    else [MkReadOnlyUpdate KeyUpdateClear]
        elUpdate (MkTupleUpdate SelectSecond (KeyUpdateItem _ edit)) _ = never edit
        elUpdate (MkTupleUpdate SelectSecond (KeyUpdateDelete item)) mr = do
            (isJust -> r1) <- mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem item ReadWhole
            return $
                if r1
                    then [MkReadOnlyUpdate $ KeyUpdateDelete item]
                    else []
        elUpdate (MkTupleUpdate SelectSecond (KeyUpdateInsertReplace item)) mr = do
            (isJust -> r1) <- mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem item ReadWhole
            return $
                if r1
                    then [MkReadOnlyUpdate $ KeyUpdateInsertReplace item]
                    else []
        elUpdate (MkTupleUpdate SelectSecond KeyUpdateClear) mr = do
            keys2 <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
            keys1 <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
            return $
                if null $ keys2 /\ keys1
                    then []
                    else [MkReadOnlyUpdate KeyUpdateClear]
        in MkEditLens {elPutEdits = elPutEditsNone, ..}

bijectionFiniteSetEditLens :: forall a b. Bijection a b -> EditLens (FiniteSetUpdate a) (FiniteSetUpdate b)
bijectionFiniteSetEditLens (MkIsomorphism ab ba) = let
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
    elGet ::
           forall m t. MonadIO m
        => MutableRead m (FiniteSetReader a)
        -> FiniteSetReader b t
        -> m t
    elGet mra KeyReadKeys = fmap (fmap ab) $ mra KeyReadKeys
    elGet mra (KeyReadItem b ReadWhole) = fmap (fmap ab) $ mra $ KeyReadItem (ba b) ReadWhole
    elUpdate ::
           forall m. MonadIO m
        => FiniteSetUpdate a
        -> MutableRead m (FiniteSetReader a)
        -> m [FiniteSetUpdate b]
    elUpdate ea _ = return $ pure $ mapFiniteSetUpdate ab ea
    elPutEdits ::
           forall m. MonadIO m
        => [FiniteSetEdit b]
        -> MutableRead m (FiniteSetReader a)
        -> m (Maybe [FiniteSetEdit a])
    elPutEdits ebs _ = return $ Just $ fmap (mapFiniteSetEdit ba) ebs
    in MkEditLens {..}

finiteSetCartesianSumEditLens ::
       forall a b. (Eq a, Eq b)
    => EditLens (PairUpdate (FiniteSetUpdate a) (FiniteSetUpdate b)) (FiniteSetUpdate (Either a b))
finiteSetCartesianSumEditLens = let
    elGet :: ReadFunction (PairUpdateReader (FiniteSetUpdate a) (FiniteSetUpdate b)) (FiniteSetReader (Either a b))
    elGet mr KeyReadKeys = do
        aa <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
        bb <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
        return $ fmap Left aa <> fmap Right bb
    elGet mr (KeyReadItem (Left a) ReadWhole) = do
        ma' <- mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem a ReadWhole
        return $ fmap Left ma'
    elGet mr (KeyReadItem (Right b) ReadWhole) = do
        mb' <- mr $ MkTupleUpdateReader SelectSecond $ KeyReadItem b ReadWhole
        return $ fmap Right mb'
    elUpdate ::
           forall m. MonadIO m
        => PairUpdate (FiniteSetUpdate a) (FiniteSetUpdate b)
        -> MutableRead m (PairUpdateReader (FiniteSetUpdate a) (FiniteSetUpdate b))
        -> m [FiniteSetUpdate (Either a b)]
    elUpdate (MkTupleUpdate SelectFirst (KeyUpdateItem _ edit)) _ = never edit
    elUpdate (MkTupleUpdate SelectFirst (KeyUpdateDelete v)) _ = return $ pure $ KeyUpdateDelete $ Left v
    elUpdate (MkTupleUpdate SelectFirst (KeyUpdateInsertReplace v)) _ = return $ pure $ KeyUpdateInsertReplace $ Left v
    elUpdate (MkTupleUpdate SelectFirst KeyUpdateClear) mr = do
        vv <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
        for (toList vv) $ \v -> return $ KeyUpdateDelete $ Left v
    elUpdate (MkTupleUpdate SelectSecond (KeyUpdateItem _ edit)) _ = never edit
    elUpdate (MkTupleUpdate SelectSecond (KeyUpdateDelete v)) _ = return $ pure $ KeyUpdateDelete $ Right v
    elUpdate (MkTupleUpdate SelectSecond (KeyUpdateInsertReplace v)) _ =
        return $ pure $ KeyUpdateInsertReplace $ Right v
    elUpdate (MkTupleUpdate SelectSecond KeyUpdateClear) mr = do
        vv <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
        for (toList vv) $ \v -> return $ KeyUpdateDelete $ Right v
    elPutEdits ::
           forall m. MonadIO m
        => [FiniteSetEdit (Either a b)]
        -> MutableRead m (PairUpdateReader (FiniteSetUpdate a) (FiniteSetUpdate b))
        -> m (Maybe [PairUpdateEdit (FiniteSetUpdate a) (FiniteSetUpdate b)])
    elPutEdits =
        elPutEditsFromSimplePutEdit $ \case
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
    in MkEditLens {..}

finiteSetCartesianProductUpdateFunction ::
       forall a b.
       EditLens (PairUpdate (FiniteSetUpdate a) (FiniteSetUpdate b)) (ReadOnlyUpdate (FiniteSetUpdate (a, b)))
finiteSetCartesianProductUpdateFunction = let
    elGet :: ReadFunction (PairUpdateReader (FiniteSetUpdate a) (FiniteSetUpdate b)) (FiniteSetReader (a, b))
    elGet mr KeyReadKeys = do
        aa <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
        bb <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
        return $ liftA2 (,) aa bb
    elGet mr (KeyReadItem (a, b) ReadWhole) =
        getComposeM $ do
            a' <- MkComposeM $ mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem a ReadWhole
            b' <- MkComposeM $ mr $ MkTupleUpdateReader SelectSecond $ KeyReadItem b ReadWhole
            return (a', b')
    elUpdate ::
           forall m. MonadIO m
        => PairUpdate (FiniteSetUpdate a) (FiniteSetUpdate b)
        -> MutableRead m (PairUpdateReader (FiniteSetUpdate a) (FiniteSetUpdate b))
        -> m [ReadOnlyUpdate (FiniteSetUpdate (a, b))]
    elUpdate (MkTupleUpdate SelectFirst KeyUpdateClear) _ = return [MkReadOnlyUpdate KeyUpdateClear]
    elUpdate (MkTupleUpdate SelectSecond KeyUpdateClear) _ = return [MkReadOnlyUpdate KeyUpdateClear]
    elUpdate (MkTupleUpdate SelectFirst (KeyUpdateItem _ update)) _ = never update
    elUpdate (MkTupleUpdate SelectSecond (KeyUpdateItem _ update)) _ = never update
    elUpdate (MkTupleUpdate SelectFirst (KeyUpdateDelete a)) mr = do
        bb <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
        return $ fmap (\b -> MkReadOnlyUpdate $ KeyUpdateDelete (a, b)) $ toList bb
    elUpdate (MkTupleUpdate SelectSecond (KeyUpdateDelete b)) mr = do
        aa <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
        return $ fmap (\a -> MkReadOnlyUpdate $ KeyUpdateDelete (a, b)) $ toList aa
    elUpdate (MkTupleUpdate SelectFirst (KeyUpdateInsertReplace a)) mr = do
        bb <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
        return $ fmap (\b -> MkReadOnlyUpdate $ KeyUpdateInsertReplace (a, b)) $ toList bb
    elUpdate (MkTupleUpdate SelectSecond (KeyUpdateInsertReplace b)) mr = do
        aa <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
        return $ fmap (\a -> MkReadOnlyUpdate $ KeyUpdateInsertReplace (a, b)) $ toList aa
    in MkEditLens {elPutEdits = elPutEditsNone, ..}

finiteSetFunctionEditLens :: forall a. EditLens (FiniteSetUpdate a) (PartialSetUpdate a)
finiteSetFunctionEditLens = let
    elGet ::
           forall m t. MonadIO m
        => MutableRead m (FiniteSetReader a)
        -> FunctionUpdateReader a (WholeUpdate Bool) t
        -> m t
    elGet mr (MkTupleUpdateReader (MkFunctionSelector a) ReadWhole) = do
        mu <- mr $ KeyReadItem a ReadWhole
        return $ isJust mu
    elUpdate ::
           forall m. MonadIO m
        => FiniteSetUpdate a
        -> MutableRead m (FiniteSetReader a)
        -> m [PartialUpdate (FunctionUpdate a (WholeUpdate Bool))]
    elUpdate (KeyUpdateItem _ update) _ = never update
    elUpdate (KeyUpdateDelete p) _ =
        return [KnownPartialUpdate $ MkTupleUpdate (MkFunctionSelector p) (MkWholeReaderUpdate False)]
    elUpdate (KeyUpdateInsertReplace p) _ =
        return [KnownPartialUpdate $ MkTupleUpdate (MkFunctionSelector p) (MkWholeReaderUpdate True)]
    elUpdate KeyUpdateClear _ = return [UnknownPartialUpdate $ \_ -> True]
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
    elPutEdits ::
           forall m. MonadIO m
        => [FunctionUpdateEdit a (WholeUpdate Bool)]
        -> MutableRead m (FiniteSetReader a)
        -> m (Maybe [FiniteSetEdit a])
    elPutEdits = elPutEditsFromSimplePutEdit elPutEdit
    in MkEditLens {..}

filterFiniteSetUpdateFunction ::
       forall a. Eq a
    => EditLens (PairUpdate (FiniteSetUpdate a) (PartialSetUpdate a)) (ReadOnlyUpdate (FiniteSetUpdate a))
filterFiniteSetUpdateFunction = let
    testItem1 ::
           forall m. MonadIO m
        => MutableRead m (PairUpdateReader (FiniteSetUpdate a) (PartialSetUpdate a))
        -> a
        -> m (Maybe a)
    testItem1 mr a = mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem a ReadWhole
    testItem2 ::
           forall m. MonadIO m
        => MutableRead m (PairUpdateReader (FiniteSetUpdate a) (PartialSetUpdate a))
        -> a
        -> m (Maybe a)
    testItem2 mr a = do
        x <- mr $ MkTupleUpdateReader SelectSecond $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole
        return $
            if x
                then Just a
                else Nothing
    elGet ::
           forall m. MonadIO m
        => MutableRead m (PairUpdateReader (FiniteSetUpdate a) (PartialSetUpdate a))
        -> MutableRead m (FiniteSetReader a)
    elGet mr (KeyReadItem a ReadWhole) = do
        mu <- testItem1 mr a
        case mu of
            Nothing -> return Nothing
            Just _ -> testItem2 mr a
    elGet mr KeyReadKeys = do
        aa <- mr $ MkTupleUpdateReader SelectFirst $ KeyReadKeys
        aa' <- for aa $ testItem2 mr
        return $ catMaybes aa'
    elUpdate ::
           forall m. MonadIO m
        => PairUpdate (FiniteSetUpdate a) (PartialSetUpdate a)
        -> MutableRead m (PairUpdateReader (FiniteSetUpdate a) (PartialSetUpdate a))
        -> m [ReadOnlyUpdate (FiniteSetUpdate a)]
    elUpdate (MkTupleUpdate SelectFirst (KeyUpdateItem _ update)) _mr = never update
    elUpdate (MkTupleUpdate SelectFirst KeyUpdateClear) _mr = return $ pure $ MkReadOnlyUpdate KeyUpdateClear
    elUpdate (MkTupleUpdate SelectFirst (KeyUpdateInsertReplace a)) mr = do
        mu <- testItem2 mr a
        return $
            case mu of
                Nothing -> []
                Just _ -> [MkReadOnlyUpdate $ KeyUpdateInsertReplace a]
    elUpdate (MkTupleUpdate SelectFirst (KeyUpdateDelete a)) mr = do
        mu <- testItem2 mr a
        return $
            case mu of
                Nothing -> []
                Just _ -> [MkReadOnlyUpdate $ KeyUpdateDelete a]
    elUpdate (MkTupleUpdate SelectSecond (KnownPartialUpdate (MkTupleUpdate (MkFunctionSelector a) (MkWholeReaderUpdate t)))) mr = do
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
    elUpdate (MkTupleUpdate SelectSecond (UnknownPartialUpdate _)) mr = do
        edits <- getReplaceEdits $ firstReadFunction mr
        return $ fmap (MkReadOnlyUpdate . editUpdate) edits
    in MkEditLens {elPutEdits = elPutEditsNone, ..}
