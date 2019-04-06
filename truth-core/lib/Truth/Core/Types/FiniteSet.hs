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
    efGet :: ReadFunctionT IdentityT (FiniteSetReader subj) (WholeReader Bool)
    efGet mr ReadWhole = lift $ fmap isJust $ mr $ KeyReadItem subj ReadWhole
    efUpdate ::
           forall m. MonadIO m
        => FiniteSetEdit subj
        -> MutableRead m (EditReader (FiniteSetEdit subj))
        -> IdentityT m [WholeEdit Bool]
    efUpdate (KeyEditItem _ edit) _ = never edit
    efUpdate (KeyDeleteItem key) _
        | key == subj = return [MkWholeEdit False]
    efUpdate (KeyDeleteItem _) _ = return []
    efUpdate (KeyInsertReplaceItem key) _
        | key == subj = return [MkWholeEdit True]
    efUpdate (KeyInsertReplaceItem _) _ = return []
    efUpdate KeyClear _ = return [MkWholeEdit False]
    elFunction :: AnEditFunction IdentityT (FiniteSetEdit subj) (WholeEdit Bool)
    elFunction = MkAnEditFunction {..}
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
    joinEditFunction = let
        efGet ::
               ReadFunctionT IdentityT (PairEditReader (FiniteSetEdit subj) (FiniteSetEdit subj)) (FiniteSetReader subj)
        efGet mr KeyReadKeys = do
            keys1 <- lift $ mr $ MkTupleEditReader SelectFirst KeyReadKeys
            keys2 <- lift $ mr $ MkTupleEditReader SelectSecond KeyReadKeys
            return $ keys1 \/ keys2
        efGet mr (KeyReadItem item ReadWhole) = do
            (isJust -> r1) <- lift $ mr $ MkTupleEditReader SelectFirst $ KeyReadItem item ReadWhole
            (isJust -> r2) <- lift $ mr $ MkTupleEditReader SelectSecond $ KeyReadItem item ReadWhole
            return $
                if r1 \/ r2
                    then Just item
                    else Nothing
        efUpdate ::
               forall m. MonadIO m
            => PairEdit (FiniteSetEdit subj) (FiniteSetEdit subj)
            -> MutableRead m (EditReader (PairEdit (FiniteSetEdit subj) (FiniteSetEdit subj)))
            -> IdentityT m [FiniteSetEdit subj]
        efUpdate (MkTupleEdit SelectFirst (KeyEditItem _ edit)) _ = never edit
        efUpdate (MkTupleEdit SelectFirst (KeyDeleteItem item)) mr = do
            (isJust -> r2) <- lift $ mr $ MkTupleEditReader SelectSecond $ KeyReadItem item ReadWhole
            return $
                if r2
                    then []
                    else [KeyDeleteItem item]
        efUpdate (MkTupleEdit SelectFirst (KeyInsertReplaceItem item)) mr = do
            (isJust -> r2) <- lift $ mr $ MkTupleEditReader SelectSecond $ KeyReadItem item ReadWhole
            return $
                if r2
                    then []
                    else [KeyInsertReplaceItem item]
        efUpdate (MkTupleEdit SelectFirst KeyClear) mr = do
            keys1 <- lift $ mr $ MkTupleEditReader SelectFirst KeyReadKeys
            keys2 <- lift $ mr $ MkTupleEditReader SelectSecond KeyReadKeys
            return $
                case (null keys1, null keys2) of
                    (True, _) -> []
                    (False, True) -> [KeyClear]
                    (False, False) -> fmap KeyDeleteItem $ toList $ difference keys1 keys2
        efUpdate (MkTupleEdit SelectSecond (KeyEditItem _ edit)) _ = never edit
        efUpdate (MkTupleEdit SelectSecond (KeyDeleteItem item)) mr = do
            (isJust -> r1) <- lift $ mr $ MkTupleEditReader SelectFirst $ KeyReadItem item ReadWhole
            return $
                if r1
                    then []
                    else [KeyDeleteItem item]
        efUpdate (MkTupleEdit SelectSecond (KeyInsertReplaceItem item)) mr = do
            (isJust -> r1) <- lift $ mr $ MkTupleEditReader SelectFirst $ KeyReadItem item ReadWhole
            return $
                if r1
                    then []
                    else [KeyInsertReplaceItem item]
        efUpdate (MkTupleEdit SelectSecond KeyClear) mr = do
            keys2 <- lift $ mr $ MkTupleEditReader SelectSecond KeyReadKeys
            keys1 <- lift $ mr $ MkTupleEditReader SelectFirst KeyReadKeys
            return $
                case (null keys2, null keys1) of
                    (True, _) -> []
                    (False, True) -> [KeyClear]
                    (False, False) -> fmap KeyDeleteItem $ toList $ difference keys2 keys1
        in MkCloseUnlift identityUnlift MkAnEditFunction {..}

instance Eq subj => MeetSemiLatticeEdit (FiniteSetEdit subj) where
    meetEditFunction = let
        efGet ::
               ReadFunctionT IdentityT (PairEditReader (FiniteSetEdit subj) (FiniteSetEdit subj)) (FiniteSetReader subj)
        efGet mr KeyReadKeys = do
            keys1 <- lift $ mr $ MkTupleEditReader SelectFirst KeyReadKeys
            keys2 <- lift $ mr $ MkTupleEditReader SelectSecond KeyReadKeys
            return $ keys1 /\ keys2
        efGet mr (KeyReadItem item ReadWhole) = do
            (isJust -> r1) <- lift $ mr $ MkTupleEditReader SelectFirst $ KeyReadItem item ReadWhole
            (isJust -> r2) <- lift $ mr $ MkTupleEditReader SelectSecond $ KeyReadItem item ReadWhole
            return $
                if r1 /\ r2
                    then Just item
                    else Nothing
        efUpdate ::
               forall m. MonadIO m
            => PairEdit (FiniteSetEdit subj) (FiniteSetEdit subj)
            -> MutableRead m (EditReader (PairEdit (FiniteSetEdit subj) (FiniteSetEdit subj)))
            -> IdentityT m [FiniteSetEdit subj]
        efUpdate (MkTupleEdit SelectFirst (KeyEditItem _ edit)) _ = never edit
        efUpdate (MkTupleEdit SelectFirst (KeyDeleteItem item)) mr = do
            (isJust -> r2) <- lift $ mr $ MkTupleEditReader SelectSecond $ KeyReadItem item ReadWhole
            return $
                if r2
                    then [KeyDeleteItem item]
                    else []
        efUpdate (MkTupleEdit SelectFirst (KeyInsertReplaceItem item)) mr = do
            (isJust -> r2) <- lift $ mr $ MkTupleEditReader SelectSecond $ KeyReadItem item ReadWhole
            return $
                if r2
                    then [KeyInsertReplaceItem item]
                    else []
        efUpdate (MkTupleEdit SelectFirst KeyClear) mr = do
            keys1 <- lift $ mr $ MkTupleEditReader SelectFirst KeyReadKeys
            keys2 <- lift $ mr $ MkTupleEditReader SelectSecond KeyReadKeys
            return $
                if null $ keys1 /\ keys2
                    then []
                    else [KeyClear]
        efUpdate (MkTupleEdit SelectSecond (KeyEditItem _ edit)) _ = never edit
        efUpdate (MkTupleEdit SelectSecond (KeyDeleteItem item)) mr = do
            (isJust -> r1) <- lift $ mr $ MkTupleEditReader SelectFirst $ KeyReadItem item ReadWhole
            return $
                if r1
                    then [KeyDeleteItem item]
                    else []
        efUpdate (MkTupleEdit SelectSecond (KeyInsertReplaceItem item)) mr = do
            (isJust -> r1) <- lift $ mr $ MkTupleEditReader SelectFirst $ KeyReadItem item ReadWhole
            return $
                if r1
                    then [KeyInsertReplaceItem item]
                    else []
        efUpdate (MkTupleEdit SelectSecond KeyClear) mr = do
            keys2 <- lift $ mr $ MkTupleEditReader SelectSecond KeyReadKeys
            keys1 <- lift $ mr $ MkTupleEditReader SelectFirst KeyReadKeys
            return $
                if null $ keys2 /\ keys1
                    then []
                    else [KeyClear]
        in MkCloseUnlift identityUnlift MkAnEditFunction {..}

bijectionFiniteSetEditLens :: forall a b. Bijection a b -> EditLens (FiniteSetEdit a) (FiniteSetEdit b)
bijectionFiniteSetEditLens (MkIsomorphism ab ba) = let
    mapFiniteSetEdit :: forall p q. (p -> q) -> FiniteSetEdit p -> FiniteSetEdit q
    mapFiniteSetEdit _ (KeyEditItem _ edit) = never edit
    mapFiniteSetEdit pq (KeyDeleteItem p) = KeyDeleteItem $ pq p
    mapFiniteSetEdit pq (KeyInsertReplaceItem p) = KeyInsertReplaceItem $ pq p
    mapFiniteSetEdit _ KeyClear = KeyClear
    efGet ::
           forall m t. MonadIO m
        => MutableRead m (FiniteSetReader a)
        -> FiniteSetReader b t
        -> IdentityT m t
    efGet mra KeyReadKeys = lift $ fmap (fmap ab) $ mra KeyReadKeys
    efGet mra (KeyReadItem b ReadWhole) = lift $ fmap (fmap ab) $ mra $ KeyReadItem (ba b) ReadWhole
    efUpdate ::
           forall m. MonadIO m
        => FiniteSetEdit a
        -> MutableRead m (EditReader (FiniteSetEdit a))
        -> IdentityT m [FiniteSetEdit b]
    efUpdate ea _ = return $ pure $ mapFiniteSetEdit ab ea
    elFunction :: AnEditFunction IdentityT (FiniteSetEdit a) (FiniteSetEdit b)
    elFunction = MkAnEditFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [FiniteSetEdit b]
        -> MutableRead m (EditReader (FiniteSetEdit a))
        -> IdentityT m (Maybe [FiniteSetEdit a])
    elPutEdits ebs _ = return $ Just $ fmap (mapFiniteSetEdit ba) ebs
    in MkCloseUnlift identityUnlift MkAnEditLens {..}
