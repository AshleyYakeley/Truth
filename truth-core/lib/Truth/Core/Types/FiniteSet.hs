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
    KeyDeleteItem :: subj -> FiniteSetEdit subj
    KeyInsertReplaceItem :: subj -> FiniteSetEdit subj
    KeyClear :: FiniteSetEdit subj
-}
type FiniteSetEdit subj = KeyEdit (FiniteSet subj) (ConstEdit subj)

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
    -> EditLens (FiniteSetEdit subj) (WholeEdit Bool)
finiteSetEditLens subj = let
    ufGet :: ReadFunctionT IdentityT (FiniteSetReader subj) (WholeReader Bool)
    ufGet mr ReadWhole = lift $ fmap isJust $ mr $ KeyReadItem subj ReadWhole
    ufUpdate ::
           forall m. MonadIO m
        => FiniteSetEdit subj
        -> MutableRead m (EditReader (FiniteSetEdit subj))
        -> IdentityT m [WholeEdit Bool]
    ufUpdate (KeyEditItem _ edit) _ = never edit
    ufUpdate (KeyDeleteItem key) _
        | key == subj = return [MkWholeEdit False]
    ufUpdate (KeyDeleteItem _) _ = return []
    ufUpdate (KeyInsertReplaceItem key) _
        | key == subj = return [MkWholeEdit True]
    ufUpdate (KeyInsertReplaceItem _) _ = return []
    ufUpdate KeyClear _ = return [MkWholeEdit False]
    elFunction :: AnUpdateFunction IdentityT (FiniteSetEdit subj) (WholeEdit Bool)
    elFunction = MkAnUpdateFunction {..}
    elPutEdit ::
           forall m. MonadIO m
        => WholeEdit Bool
        -> IdentityT m (Maybe [FiniteSetEdit subj])
    elPutEdit (MkWholeEdit False) = return $ Just [KeyDeleteItem subj]
    elPutEdit (MkWholeEdit True) = return $ Just [KeyInsertReplaceItem subj]
    elPutEdits ::
           forall m. MonadIO m
        => [WholeEdit Bool]
        -> MutableRead m (EditReader (FiniteSetEdit subj))
        -> IdentityT m (Maybe [FiniteSetEdit subj])
    elPutEdits = elPutEditsFromSimplePutEdit elPutEdit
    in MkCloseUnlift identityUnlift MkAnEditLens {..}

instance Eq subj => JoinSemiLatticeEdit (FiniteSetEdit subj) where
    joinUpdateFunction = let
        ufGet ::
               ReadFunctionT IdentityT (PairEditReader (FiniteSetEdit subj) (FiniteSetEdit subj)) (FiniteSetReader subj)
        ufGet mr KeyReadKeys = do
            keys1 <- lift $ mr $ MkTupleEditReader SelectFirst KeyReadKeys
            keys2 <- lift $ mr $ MkTupleEditReader SelectSecond KeyReadKeys
            return $ keys1 \/ keys2
        ufGet mr (KeyReadItem item ReadWhole) = do
            (isJust -> r1) <- lift $ mr $ MkTupleEditReader SelectFirst $ KeyReadItem item ReadWhole
            (isJust -> r2) <- lift $ mr $ MkTupleEditReader SelectSecond $ KeyReadItem item ReadWhole
            return $
                if r1 \/ r2
                    then Just item
                    else Nothing
        ufUpdate ::
               forall m. MonadIO m
            => PairEdit (FiniteSetEdit subj) (FiniteSetEdit subj)
            -> MutableRead m (EditReader (PairEdit (FiniteSetEdit subj) (FiniteSetEdit subj)))
            -> IdentityT m [FiniteSetEdit subj]
        ufUpdate (MkTupleEdit SelectFirst (KeyEditItem _ edit)) _ = never edit
        ufUpdate (MkTupleEdit SelectFirst (KeyDeleteItem item)) mr = do
            (isJust -> r2) <- lift $ mr $ MkTupleEditReader SelectSecond $ KeyReadItem item ReadWhole
            return $
                if r2
                    then []
                    else [KeyDeleteItem item]
        ufUpdate (MkTupleEdit SelectFirst (KeyInsertReplaceItem item)) mr = do
            (isJust -> r2) <- lift $ mr $ MkTupleEditReader SelectSecond $ KeyReadItem item ReadWhole
            return $
                if r2
                    then []
                    else [KeyInsertReplaceItem item]
        ufUpdate (MkTupleEdit SelectFirst KeyClear) mr = do
            keys1 <- lift $ mr $ MkTupleEditReader SelectFirst KeyReadKeys
            keys2 <- lift $ mr $ MkTupleEditReader SelectSecond KeyReadKeys
            return $
                case (null keys1, null keys2) of
                    (True, _) -> []
                    (False, True) -> [KeyClear]
                    (False, False) -> fmap KeyDeleteItem $ toList $ difference keys1 keys2
        ufUpdate (MkTupleEdit SelectSecond (KeyEditItem _ edit)) _ = never edit
        ufUpdate (MkTupleEdit SelectSecond (KeyDeleteItem item)) mr = do
            (isJust -> r1) <- lift $ mr $ MkTupleEditReader SelectFirst $ KeyReadItem item ReadWhole
            return $
                if r1
                    then []
                    else [KeyDeleteItem item]
        ufUpdate (MkTupleEdit SelectSecond (KeyInsertReplaceItem item)) mr = do
            (isJust -> r1) <- lift $ mr $ MkTupleEditReader SelectFirst $ KeyReadItem item ReadWhole
            return $
                if r1
                    then []
                    else [KeyInsertReplaceItem item]
        ufUpdate (MkTupleEdit SelectSecond KeyClear) mr = do
            keys2 <- lift $ mr $ MkTupleEditReader SelectSecond KeyReadKeys
            keys1 <- lift $ mr $ MkTupleEditReader SelectFirst KeyReadKeys
            return $
                case (null keys2, null keys1) of
                    (True, _) -> []
                    (False, True) -> [KeyClear]
                    (False, False) -> fmap KeyDeleteItem $ toList $ difference keys2 keys1
        in MkCloseUnlift identityUnlift MkAnUpdateFunction {..}

instance Eq subj => MeetSemiLatticeEdit (FiniteSetEdit subj) where
    meetUpdateFunction = let
        ufGet ::
               ReadFunctionT IdentityT (PairEditReader (FiniteSetEdit subj) (FiniteSetEdit subj)) (FiniteSetReader subj)
        ufGet mr KeyReadKeys = do
            keys1 <- lift $ mr $ MkTupleEditReader SelectFirst KeyReadKeys
            keys2 <- lift $ mr $ MkTupleEditReader SelectSecond KeyReadKeys
            return $ keys1 /\ keys2
        ufGet mr (KeyReadItem item ReadWhole) = do
            (isJust -> r1) <- lift $ mr $ MkTupleEditReader SelectFirst $ KeyReadItem item ReadWhole
            (isJust -> r2) <- lift $ mr $ MkTupleEditReader SelectSecond $ KeyReadItem item ReadWhole
            return $
                if r1 /\ r2
                    then Just item
                    else Nothing
        ufUpdate ::
               forall m. MonadIO m
            => PairEdit (FiniteSetEdit subj) (FiniteSetEdit subj)
            -> MutableRead m (EditReader (PairEdit (FiniteSetEdit subj) (FiniteSetEdit subj)))
            -> IdentityT m [FiniteSetEdit subj]
        ufUpdate (MkTupleEdit SelectFirst (KeyEditItem _ edit)) _ = never edit
        ufUpdate (MkTupleEdit SelectFirst (KeyDeleteItem item)) mr = do
            (isJust -> r2) <- lift $ mr $ MkTupleEditReader SelectSecond $ KeyReadItem item ReadWhole
            return $
                if r2
                    then [KeyDeleteItem item]
                    else []
        ufUpdate (MkTupleEdit SelectFirst (KeyInsertReplaceItem item)) mr = do
            (isJust -> r2) <- lift $ mr $ MkTupleEditReader SelectSecond $ KeyReadItem item ReadWhole
            return $
                if r2
                    then [KeyInsertReplaceItem item]
                    else []
        ufUpdate (MkTupleEdit SelectFirst KeyClear) mr = do
            keys1 <- lift $ mr $ MkTupleEditReader SelectFirst KeyReadKeys
            keys2 <- lift $ mr $ MkTupleEditReader SelectSecond KeyReadKeys
            return $
                if null $ keys1 /\ keys2
                    then []
                    else [KeyClear]
        ufUpdate (MkTupleEdit SelectSecond (KeyEditItem _ edit)) _ = never edit
        ufUpdate (MkTupleEdit SelectSecond (KeyDeleteItem item)) mr = do
            (isJust -> r1) <- lift $ mr $ MkTupleEditReader SelectFirst $ KeyReadItem item ReadWhole
            return $
                if r1
                    then [KeyDeleteItem item]
                    else []
        ufUpdate (MkTupleEdit SelectSecond (KeyInsertReplaceItem item)) mr = do
            (isJust -> r1) <- lift $ mr $ MkTupleEditReader SelectFirst $ KeyReadItem item ReadWhole
            return $
                if r1
                    then [KeyInsertReplaceItem item]
                    else []
        ufUpdate (MkTupleEdit SelectSecond KeyClear) mr = do
            keys2 <- lift $ mr $ MkTupleEditReader SelectSecond KeyReadKeys
            keys1 <- lift $ mr $ MkTupleEditReader SelectFirst KeyReadKeys
            return $
                if null $ keys2 /\ keys1
                    then []
                    else [KeyClear]
        in MkCloseUnlift identityUnlift MkAnUpdateFunction {..}

bijectionFiniteSetEditLens :: forall a b. Bijection a b -> EditLens (FiniteSetEdit a) (FiniteSetEdit b)
bijectionFiniteSetEditLens (MkIsomorphism ab ba) = let
    mapFiniteSetEdit :: forall p q. (p -> q) -> FiniteSetEdit p -> FiniteSetEdit q
    mapFiniteSetEdit _ (KeyEditItem _ edit) = never edit
    mapFiniteSetEdit pq (KeyDeleteItem p) = KeyDeleteItem $ pq p
    mapFiniteSetEdit pq (KeyInsertReplaceItem p) = KeyInsertReplaceItem $ pq p
    mapFiniteSetEdit _ KeyClear = KeyClear
    ufGet ::
           forall m t. MonadIO m
        => MutableRead m (FiniteSetReader a)
        -> FiniteSetReader b t
        -> IdentityT m t
    ufGet mra KeyReadKeys = lift $ fmap (fmap ab) $ mra KeyReadKeys
    ufGet mra (KeyReadItem b ReadWhole) = lift $ fmap (fmap ab) $ mra $ KeyReadItem (ba b) ReadWhole
    ufUpdate ::
           forall m. MonadIO m
        => FiniteSetEdit a
        -> MutableRead m (EditReader (FiniteSetEdit a))
        -> IdentityT m [FiniteSetEdit b]
    ufUpdate ea _ = return $ pure $ mapFiniteSetEdit ab ea
    elFunction :: AnUpdateFunction IdentityT (FiniteSetEdit a) (FiniteSetEdit b)
    elFunction = MkAnUpdateFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [FiniteSetEdit b]
        -> MutableRead m (EditReader (FiniteSetEdit a))
        -> IdentityT m (Maybe [FiniteSetEdit a])
    elPutEdits ebs _ = return $ Just $ fmap (mapFiniteSetEdit ba) ebs
    in MkCloseUnlift identityUnlift MkAnEditLens {..}
