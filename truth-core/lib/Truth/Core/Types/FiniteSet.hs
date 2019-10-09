{-# OPTIONS -fno-warn-orphans #-}

module Truth.Core.Types.FiniteSet where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.Function
import Truth.Core.Types.Key
import Truth.Core.Types.Lattice
import Truth.Core.Types.Pair
import Truth.Core.Types.Set
import Truth.Core.Types.Tuple
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
    KeyEditItem :: subj -> ConstEdit subj -> FiniteSetEdit subj
    KeyEditDelete :: subj -> FiniteSetEdit subj
    KeyEditInsertReplace :: subj -> FiniteSetEdit subj
    KeyEditClear :: FiniteSetEdit subj
-}
type FiniteSetEdit subj = KeyEdit (FiniteSet subj) (ConstEdit subj)

{- equivalent to:
data FiniteSetUpdate subj where
    KeyUpdateItem :: subj -> ConstUpdate subj -> FiniteSetUpdate subj
    KeyUpdateDelete :: subj -> FiniteSetUpdate subj
    KeyUpdateInsertReplace :: subj -> FiniteSetUpdate subj
    KeyUpdateClear :: FiniteSetUpdate subj
-}
type FiniteSetUpdate subj = KeyUpdate (FiniteSet subj) (ConstUpdate subj)

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
    ufGet :: ReadFunctionT IdentityT (FiniteSetReader subj) (WholeReader Bool)
    ufGet mr ReadWhole = lift $ fmap isJust $ mr $ KeyReadItem subj ReadWhole
    ufUpdate ::
           forall m. MonadIO m
        => FiniteSetUpdate subj
        -> MutableRead m (FiniteSetReader subj)
        -> IdentityT m [WholeUpdate Bool]
    ufUpdate (KeyUpdateItem _ update) _ = never update
    ufUpdate (KeyUpdateDelete key) _
        | key == subj = return [MkWholeReaderUpdate False]
    ufUpdate (KeyUpdateDelete _) _ = return []
    ufUpdate (KeyUpdateInsertReplace key) _
        | key == subj = return [MkWholeReaderUpdate True]
    ufUpdate (KeyUpdateInsertReplace _) _ = return []
    ufUpdate KeyUpdateClear _ = return [MkWholeReaderUpdate False]
    elFunction :: AnUpdateFunction IdentityT (FiniteSetUpdate subj) (WholeUpdate Bool)
    elFunction = MkAnUpdateFunction {..}
    elPutEdit ::
           forall m. MonadIO m
        => WholeEdit Bool
        -> IdentityT m (Maybe [FiniteSetEdit subj])
    elPutEdit (MkWholeReaderEdit False) = return $ Just [KeyEditDelete subj]
    elPutEdit (MkWholeReaderEdit True) = return $ Just [KeyEditInsertReplace subj]
    elPutEdits ::
           forall m. MonadIO m
        => [WholeEdit Bool]
        -> MutableRead m (FiniteSetReader subj)
        -> IdentityT m (Maybe [FiniteSetEdit subj])
    elPutEdits = elPutEditsFromSimplePutEdit elPutEdit
    in MkRunnableT2 wUnIdentityT MkAnEditLens {..}

instance Eq subj => JoinSemiLatticeUpdateFunction (FiniteSetUpdate subj) where
    joinUpdateFunction = let
        ufGet ::
               ReadFunctionT IdentityT (PairUpdateReader (FiniteSetUpdate subj) (FiniteSetUpdate subj)) (FiniteSetReader subj)
        ufGet mr KeyReadKeys = do
            keys1 <- lift $ mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
            keys2 <- lift $ mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
            return $ keys1 \/ keys2
        ufGet mr (KeyReadItem item ReadWhole) = do
            (isJust -> r1) <- lift $ mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem item ReadWhole
            (isJust -> r2) <- lift $ mr $ MkTupleUpdateReader SelectSecond $ KeyReadItem item ReadWhole
            return $
                if r1 \/ r2
                    then Just item
                    else Nothing
        ufUpdate ::
               forall m. MonadIO m
            => PairUpdate (FiniteSetUpdate subj) (FiniteSetUpdate subj)
            -> MutableRead m (PairUpdateReader (FiniteSetUpdate subj) (FiniteSetUpdate subj))
            -> IdentityT m [FiniteSetUpdate subj]
        ufUpdate (MkTupleUpdate SelectFirst (KeyUpdateItem _ edit)) _ = never edit
        ufUpdate (MkTupleUpdate SelectFirst (KeyUpdateDelete item)) mr = do
            (isJust -> r2) <- lift $ mr $ MkTupleUpdateReader SelectSecond $ KeyReadItem item ReadWhole
            return $
                if r2
                    then []
                    else [KeyUpdateDelete item]
        ufUpdate (MkTupleUpdate SelectFirst (KeyUpdateInsertReplace item)) mr = do
            (isJust -> r2) <- lift $ mr $ MkTupleUpdateReader SelectSecond $ KeyReadItem item ReadWhole
            return $
                if r2
                    then []
                    else [KeyUpdateInsertReplace item]
        ufUpdate (MkTupleUpdate SelectFirst KeyUpdateClear) mr = do
            keys1 <- lift $ mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
            keys2 <- lift $ mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
            return $
                case (null keys1, null keys2) of
                    (True, _) -> []
                    (False, True) -> [KeyUpdateClear]
                    (False, False) -> fmap KeyUpdateDelete $ toList $ difference keys1 keys2
        ufUpdate (MkTupleUpdate SelectSecond (KeyUpdateItem _ edit)) _ = never edit
        ufUpdate (MkTupleUpdate SelectSecond (KeyUpdateDelete item)) mr = do
            (isJust -> r1) <- lift $ mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem item ReadWhole
            return $
                if r1
                    then []
                    else [KeyUpdateDelete item]
        ufUpdate (MkTupleUpdate SelectSecond (KeyUpdateInsertReplace item)) mr = do
            (isJust -> r1) <- lift $ mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem item ReadWhole
            return $
                if r1
                    then []
                    else [KeyUpdateInsertReplace item]
        ufUpdate (MkTupleUpdate SelectSecond KeyUpdateClear) mr = do
            keys2 <- lift $ mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
            keys1 <- lift $ mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
            return $
                case (null keys2, null keys1) of
                    (True, _) -> []
                    (False, True) -> [KeyUpdateClear]
                    (False, False) -> fmap KeyUpdateDelete $ toList $ difference keys2 keys1
        in MkRunnableT2 wUnIdentityT MkAnUpdateFunction {..}

instance Eq subj => MeetSemiLatticeUpdateFunction (FiniteSetUpdate subj) where
    meetUpdateFunction = let
        ufGet ::
               ReadFunctionT IdentityT (PairUpdateReader (FiniteSetUpdate subj) (FiniteSetUpdate subj)) (FiniteSetReader subj)
        ufGet mr KeyReadKeys = do
            keys1 <- lift $ mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
            keys2 <- lift $ mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
            return $ keys1 /\ keys2
        ufGet mr (KeyReadItem item ReadWhole) = do
            (isJust -> r1) <- lift $ mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem item ReadWhole
            (isJust -> r2) <- lift $ mr $ MkTupleUpdateReader SelectSecond $ KeyReadItem item ReadWhole
            return $
                if r1 /\ r2
                    then Just item
                    else Nothing
        ufUpdate ::
               forall m. MonadIO m
            => PairUpdate (FiniteSetUpdate subj) (FiniteSetUpdate subj)
            -> MutableRead m (PairUpdateReader (FiniteSetUpdate subj) (FiniteSetUpdate subj))
            -> IdentityT m [FiniteSetUpdate subj]
        ufUpdate (MkTupleUpdate SelectFirst (KeyUpdateItem _ edit)) _ = never edit
        ufUpdate (MkTupleUpdate SelectFirst (KeyUpdateDelete item)) mr = do
            (isJust -> r2) <- lift $ mr $ MkTupleUpdateReader SelectSecond $ KeyReadItem item ReadWhole
            return $
                if r2
                    then [KeyUpdateDelete item]
                    else []
        ufUpdate (MkTupleUpdate SelectFirst (KeyUpdateInsertReplace item)) mr = do
            (isJust -> r2) <- lift $ mr $ MkTupleUpdateReader SelectSecond $ KeyReadItem item ReadWhole
            return $
                if r2
                    then [KeyUpdateInsertReplace item]
                    else []
        ufUpdate (MkTupleUpdate SelectFirst KeyUpdateClear) mr = do
            keys1 <- lift $ mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
            keys2 <- lift $ mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
            return $
                if null $ keys1 /\ keys2
                    then []
                    else [KeyUpdateClear]
        ufUpdate (MkTupleUpdate SelectSecond (KeyUpdateItem _ edit)) _ = never edit
        ufUpdate (MkTupleUpdate SelectSecond (KeyUpdateDelete item)) mr = do
            (isJust -> r1) <- lift $ mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem item ReadWhole
            return $
                if r1
                    then [KeyUpdateDelete item]
                    else []
        ufUpdate (MkTupleUpdate SelectSecond (KeyUpdateInsertReplace item)) mr = do
            (isJust -> r1) <- lift $ mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem item ReadWhole
            return $
                if r1
                    then [KeyUpdateInsertReplace item]
                    else []
        ufUpdate (MkTupleUpdate SelectSecond KeyUpdateClear) mr = do
            keys2 <- lift $ mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
            keys1 <- lift $ mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
            return $
                if null $ keys2 /\ keys1
                    then []
                    else [KeyUpdateClear]
        in MkRunnableT2 wUnIdentityT MkAnUpdateFunction {..}

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
    ufGet ::
           forall m t. MonadIO m
        => MutableRead m (FiniteSetReader a)
        -> FiniteSetReader b t
        -> IdentityT m t
    ufGet mra KeyReadKeys = lift $ fmap (fmap ab) $ mra KeyReadKeys
    ufGet mra (KeyReadItem b ReadWhole) = lift $ fmap (fmap ab) $ mra $ KeyReadItem (ba b) ReadWhole
    ufUpdate ::
           forall m. MonadIO m
        => FiniteSetUpdate a
        -> MutableRead m (FiniteSetReader a)
        -> IdentityT m [FiniteSetUpdate b]
    ufUpdate ea _ = return $ pure $ mapFiniteSetUpdate ab ea
    elFunction :: AnUpdateFunction IdentityT (FiniteSetUpdate a) (FiniteSetUpdate b)
    elFunction = MkAnUpdateFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [FiniteSetEdit b]
        -> MutableRead m (FiniteSetReader a)
        -> IdentityT m (Maybe [FiniteSetEdit a])
    elPutEdits ebs _ = return $ Just $ fmap (mapFiniteSetEdit ba) ebs
    in MkRunnableT2 wUnIdentityT MkAnEditLens {..}

finiteSetCartesianSumEditLens ::
       forall a b. (Eq a, Eq b)
    => EditLens (PairUpdate (FiniteSetUpdate a) (FiniteSetUpdate b)) (FiniteSetUpdate (Either a b))
finiteSetCartesianSumEditLens = let
    ufGet ::
           ReadFunctionT IdentityT (PairUpdateReader (FiniteSetUpdate a) (FiniteSetUpdate b)) (FiniteSetReader (Either a b))
    ufGet mr KeyReadKeys =
        lift $ do
            aa <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
            bb <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
            return $ fmap Left aa <> fmap Right bb
    ufGet mr (KeyReadItem (Left a) ReadWhole) =
        lift $ do
            ma' <- mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem a ReadWhole
            return $ fmap Left ma'
    ufGet mr (KeyReadItem (Right b) ReadWhole) =
        lift $ do
            mb' <- mr $ MkTupleUpdateReader SelectSecond $ KeyReadItem b ReadWhole
            return $ fmap Right mb'
    ufUpdate ::
           forall m. MonadIO m
        => PairUpdate (FiniteSetUpdate a) (FiniteSetUpdate b)
        -> MutableRead m (PairUpdateReader (FiniteSetUpdate a) (FiniteSetUpdate b))
        -> IdentityT m [FiniteSetUpdate (Either a b)]
    ufUpdate (MkTupleUpdate SelectFirst (KeyUpdateItem _ edit)) _ = never edit
    ufUpdate (MkTupleUpdate SelectFirst (KeyUpdateDelete v)) _ = return $ pure $ KeyUpdateDelete $ Left v
    ufUpdate (MkTupleUpdate SelectFirst (KeyUpdateInsertReplace v)) _ = return $ pure $ KeyUpdateInsertReplace $ Left v
    ufUpdate (MkTupleUpdate SelectFirst KeyUpdateClear) mr =
        lift $ do
            vv <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
            for (toList vv) $ \v -> return $ KeyUpdateDelete $ Left v
    ufUpdate (MkTupleUpdate SelectSecond (KeyUpdateItem _ edit)) _ = never edit
    ufUpdate (MkTupleUpdate SelectSecond (KeyUpdateDelete v)) _ = return $ pure $ KeyUpdateDelete $ Right v
    ufUpdate (MkTupleUpdate SelectSecond (KeyUpdateInsertReplace v)) _ =
        return $ pure $ KeyUpdateInsertReplace $ Right v
    ufUpdate (MkTupleUpdate SelectSecond KeyUpdateClear) mr =
        lift $ do
            vv <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
            for (toList vv) $ \v -> return $ KeyUpdateDelete $ Right v
    elFunction ::
           AnUpdateFunction IdentityT (PairUpdate (FiniteSetUpdate a) (FiniteSetUpdate b)) (FiniteSetUpdate (Either a b))
    elFunction = MkAnUpdateFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [FiniteSetEdit (Either a b)]
        -> MutableRead m (PairUpdateReader (FiniteSetUpdate a) (FiniteSetUpdate b))
        -> IdentityT m (Maybe [PairUpdateEdit (FiniteSetUpdate a) (FiniteSetUpdate b)])
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
    in MkRunnableT2 wUnIdentityT MkAnEditLens {..}

finiteSetCartesianProductUpdateFunction ::
       forall a b. UpdateFunction (PairUpdate (FiniteSetUpdate a) (FiniteSetUpdate b)) (FiniteSetUpdate (a, b))
finiteSetCartesianProductUpdateFunction = let
    ufGet :: ReadFunctionT IdentityT (PairUpdateReader (FiniteSetUpdate a) (FiniteSetUpdate b)) (FiniteSetReader (a, b))
    ufGet mr KeyReadKeys =
        lift $ do
            aa <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
            bb <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
            return $ liftA2 (,) aa bb
    ufGet mr (KeyReadItem (a, b) ReadWhole) =
        lift $
        getComposeM $ do
            a' <- MkComposeM $ mr $ MkTupleUpdateReader SelectFirst $ KeyReadItem a ReadWhole
            b' <- MkComposeM $ mr $ MkTupleUpdateReader SelectSecond $ KeyReadItem b ReadWhole
            return (a', b')
    ufUpdate ::
           forall m. MonadIO m
        => PairUpdate (FiniteSetUpdate a) (FiniteSetUpdate b)
        -> MutableRead m (PairUpdateReader (FiniteSetUpdate a) (FiniteSetUpdate b))
        -> IdentityT m [FiniteSetUpdate (a, b)]
    ufUpdate (MkTupleUpdate SelectFirst KeyUpdateClear) _ = return [KeyUpdateClear]
    ufUpdate (MkTupleUpdate SelectSecond KeyUpdateClear) _ = return [KeyUpdateClear]
    ufUpdate (MkTupleUpdate SelectFirst (KeyUpdateItem _ update)) _ = never update
    ufUpdate (MkTupleUpdate SelectSecond (KeyUpdateItem _ update)) _ = never update
    ufUpdate (MkTupleUpdate SelectFirst (KeyUpdateDelete a)) mr =
        lift $ do
            bb <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
            return $ fmap (\b -> KeyUpdateDelete (a, b)) $ toList bb
    ufUpdate (MkTupleUpdate SelectSecond (KeyUpdateDelete b)) mr =
        lift $ do
            aa <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
            return $ fmap (\a -> KeyUpdateDelete (a, b)) $ toList aa
    ufUpdate (MkTupleUpdate SelectFirst (KeyUpdateInsertReplace a)) mr =
        lift $ do
            bb <- mr $ MkTupleUpdateReader SelectSecond KeyReadKeys
            return $ fmap (\b -> KeyUpdateInsertReplace (a, b)) $ toList bb
    ufUpdate (MkTupleUpdate SelectSecond (KeyUpdateInsertReplace b)) mr =
        lift $ do
            aa <- mr $ MkTupleUpdateReader SelectFirst KeyReadKeys
            return $ fmap (\a -> KeyUpdateInsertReplace (a, b)) $ toList aa
    in MkRunnableT2 wUnIdentityT MkAnUpdateFunction {..}

finiteSetFunctionEditLens :: forall a. EditLens (FiniteSetUpdate a) (PartialSetUpdate a)
finiteSetFunctionEditLens = let
    ufGet ::
           forall m t. MonadIO m
        => MutableRead m (FiniteSetReader a)
        -> FunctionUpdateReader a (WholeUpdate Bool) t
        -> IdentityT m t
    ufGet mr (MkTupleUpdateReader (MkFunctionSelector a) ReadWhole) =
        lift $ do
            mu <- mr $ KeyReadItem a ReadWhole
            return $ isJust mu
    ufUpdate ::
           forall m. MonadIO m
        => FiniteSetUpdate a
        -> MutableRead m (FiniteSetReader a)
        -> IdentityT m [PartialUpdate (FunctionUpdate a (WholeUpdate Bool))]
    ufUpdate (KeyUpdateItem _ update) _ = never update
    ufUpdate (KeyUpdateDelete p) _ =
        return [KnownPartialUpdate $ MkTupleUpdate (MkFunctionSelector p) (MkWholeReaderUpdate False)]
    ufUpdate (KeyUpdateInsertReplace p) _ =
        return [KnownPartialUpdate $ MkTupleUpdate (MkFunctionSelector p) (MkWholeReaderUpdate True)]
    ufUpdate KeyUpdateClear _ = return [UnknownPartialUpdate $ \_ -> True]
    elFunction :: AnUpdateFunction IdentityT (FiniteSetUpdate a) (PartialUpdate (FunctionUpdate a (WholeUpdate Bool)))
    elFunction = MkAnUpdateFunction {..}
    elPutEdit ::
           forall m. MonadIO m
        => FunctionUpdateEdit a (WholeUpdate Bool)
        -> IdentityT m (Maybe [FiniteSetEdit a])
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
        -> IdentityT m (Maybe [FiniteSetEdit a])
    elPutEdits = elPutEditsFromSimplePutEdit elPutEdit
    in MkRunnableT2 wUnIdentityT MkAnEditLens {..}

filterFiniteSetUpdateFunction ::
       forall a. Eq a
    => UpdateFunction (PairUpdate (FiniteSetUpdate a) (PartialSetUpdate a)) (FiniteSetUpdate a)
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
    ufGet ::
           forall m. MonadIO m
        => MutableRead m (PairUpdateReader (FiniteSetUpdate a) (PartialSetUpdate a))
        -> MutableRead (IdentityT m) (FiniteSetReader a)
    ufGet mr (KeyReadItem a ReadWhole) =
        lift $ do
            mu <- testItem1 mr a
            case mu of
                Nothing -> return Nothing
                Just _ -> testItem2 mr a
    ufGet mr KeyReadKeys =
        lift $ do
            aa <- mr $ MkTupleUpdateReader SelectFirst $ KeyReadKeys
            aa' <- for aa $ testItem2 mr
            return $ catMaybes aa'
    ufUpdate ::
           forall m. MonadIO m
        => PairUpdate (FiniteSetUpdate a) (PartialSetUpdate a)
        -> MutableRead m (PairUpdateReader (FiniteSetUpdate a) (PartialSetUpdate a))
        -> IdentityT m [FiniteSetUpdate a]
    ufUpdate (MkTupleUpdate SelectFirst (KeyUpdateItem _ update)) _mr = never update
    ufUpdate (MkTupleUpdate SelectFirst KeyUpdateClear) _mr = return $ pure $ KeyUpdateClear
    ufUpdate (MkTupleUpdate SelectFirst (KeyUpdateInsertReplace a)) mr =
        lift $ do
            mu <- testItem2 mr a
            return $
                case mu of
                    Nothing -> []
                    Just _ -> [KeyUpdateInsertReplace a]
    ufUpdate (MkTupleUpdate SelectFirst (KeyUpdateDelete a)) mr =
        lift $ do
            mu <- testItem2 mr a
            return $
                case mu of
                    Nothing -> []
                    Just _ -> [KeyUpdateDelete a]
    ufUpdate (MkTupleUpdate SelectSecond (KnownPartialUpdate (MkTupleUpdate (MkFunctionSelector a) (MkWholeReaderUpdate t)))) mr =
        lift $ do
            mu <- testItem1 mr a
            return $
                pure $
                case mu of
                    Nothing -> KeyUpdateDelete a
                    Just _ ->
                        if t
                            then KeyUpdateInsertReplace a
                            else KeyUpdateDelete a
    ufUpdate (MkTupleUpdate SelectSecond (UnknownPartialUpdate _)) mr =
        lift $ do
            edits <- getReplaceEdits $ firstReadFunction mr
            return $ fmap editUpdate edits
    in MkRunnableT2 wUnIdentityT MkAnUpdateFunction {..}
