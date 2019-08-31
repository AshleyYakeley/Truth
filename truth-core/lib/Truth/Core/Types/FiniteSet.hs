{-# OPTIONS -fno-warn-orphans #-}

module Truth.Core.Types.FiniteSet where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.Key
import Truth.Core.Types.Lattice
import Truth.Core.Types.Pair
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
    in MkCloseUnlift identityUnlift MkAnEditLens {..}

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
        in MkCloseUnlift identityUnlift MkAnUpdateFunction {..}

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
        in MkCloseUnlift identityUnlift MkAnUpdateFunction {..}

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
    in MkCloseUnlift identityUnlift MkAnEditLens {..}
