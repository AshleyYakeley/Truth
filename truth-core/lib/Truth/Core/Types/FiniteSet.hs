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
    -> EditLens' (FiniteSetEdit subj) (WholeEdit Bool)
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
        -> MutableRead m (EditReader (FiniteSetEdit subj))
        -> IdentityT m (Maybe [FiniteSetEdit subj])
    elPutEdit (MkWholeEdit False) _ = return $ Just [KeyDeleteItem subj]
    elPutEdit (MkWholeEdit True) _ = return $ Just [KeyInsertReplaceItem subj]
    in MkCloseUnlift identityUnlift MkAnEditLens {..}

instance Eq subj => JoinSemiLatticeEdit (FiniteSetEdit subj) where
    joinEditFunction = let
        efGet ::
               ReadFunctionT IdentityT (PairEditReader (FiniteSetEdit subj) (FiniteSetEdit subj)) (FiniteSetReader subj)
        efGet mr KeyReadKeys = do
            keys1 <- lift $ mr $ MkTupleEditReader EditFirst KeyReadKeys
            keys2 <- lift $ mr $ MkTupleEditReader EditSecond KeyReadKeys
            return $ keys1 \/ keys2
        efGet mr (KeyReadItem item ReadWhole) = do
            (isJust -> r1) <- lift $ mr $ MkTupleEditReader EditFirst $ KeyReadItem item ReadWhole
            (isJust -> r2) <- lift $ mr $ MkTupleEditReader EditSecond $ KeyReadItem item ReadWhole
            return $
                if r1 \/ r2
                    then Just item
                    else Nothing
        efUpdate ::
               forall m. MonadIO m
            => PairEdit (FiniteSetEdit subj) (FiniteSetEdit subj)
            -> MutableRead m (EditReader (PairEdit (FiniteSetEdit subj) (FiniteSetEdit subj)))
            -> IdentityT m [FiniteSetEdit subj]
        efUpdate (MkTupleEdit EditFirst (KeyEditItem _ edit)) _ = never edit
        efUpdate (MkTupleEdit EditFirst (KeyDeleteItem item)) mr = do
            (isJust -> r2) <- lift $ mr $ MkTupleEditReader EditSecond $ KeyReadItem item ReadWhole
            return $
                if r2
                    then []
                    else [KeyDeleteItem item]
        efUpdate (MkTupleEdit EditFirst (KeyInsertReplaceItem item)) mr = do
            (isJust -> r2) <- lift $ mr $ MkTupleEditReader EditSecond $ KeyReadItem item ReadWhole
            return $
                if r2
                    then []
                    else [KeyInsertReplaceItem item]
        efUpdate (MkTupleEdit EditFirst KeyClear) mr = do
            keys1 <- lift $ mr $ MkTupleEditReader EditFirst KeyReadKeys
            keys2 <- lift $ mr $ MkTupleEditReader EditSecond KeyReadKeys
            return $
                case (null keys1, null keys2) of
                    (True, _) -> []
                    (False, True) -> [KeyClear]
                    (False, False) -> fmap KeyDeleteItem $ toList $ difference keys1 keys2
        efUpdate (MkTupleEdit EditSecond (KeyEditItem _ edit)) _ = never edit
        efUpdate (MkTupleEdit EditSecond (KeyDeleteItem item)) mr = do
            (isJust -> r1) <- lift $ mr $ MkTupleEditReader EditFirst $ KeyReadItem item ReadWhole
            return $
                if r1
                    then []
                    else [KeyDeleteItem item]
        efUpdate (MkTupleEdit EditSecond (KeyInsertReplaceItem item)) mr = do
            (isJust -> r1) <- lift $ mr $ MkTupleEditReader EditFirst $ KeyReadItem item ReadWhole
            return $
                if r1
                    then []
                    else [KeyInsertReplaceItem item]
        efUpdate (MkTupleEdit EditSecond KeyClear) mr = do
            keys2 <- lift $ mr $ MkTupleEditReader EditSecond KeyReadKeys
            keys1 <- lift $ mr $ MkTupleEditReader EditFirst KeyReadKeys
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
            keys1 <- lift $ mr $ MkTupleEditReader EditFirst KeyReadKeys
            keys2 <- lift $ mr $ MkTupleEditReader EditSecond KeyReadKeys
            return $ keys1 /\ keys2
        efGet mr (KeyReadItem item ReadWhole) = do
            (isJust -> r1) <- lift $ mr $ MkTupleEditReader EditFirst $ KeyReadItem item ReadWhole
            (isJust -> r2) <- lift $ mr $ MkTupleEditReader EditSecond $ KeyReadItem item ReadWhole
            return $
                if r1 /\ r2
                    then Just item
                    else Nothing
        efUpdate ::
               forall m. MonadIO m
            => PairEdit (FiniteSetEdit subj) (FiniteSetEdit subj)
            -> MutableRead m (EditReader (PairEdit (FiniteSetEdit subj) (FiniteSetEdit subj)))
            -> IdentityT m [FiniteSetEdit subj]
        efUpdate (MkTupleEdit EditFirst (KeyEditItem _ edit)) _ = never edit
        efUpdate (MkTupleEdit EditFirst (KeyDeleteItem item)) mr = do
            (isJust -> r2) <- lift $ mr $ MkTupleEditReader EditSecond $ KeyReadItem item ReadWhole
            return $
                if r2
                    then [KeyDeleteItem item]
                    else []
        efUpdate (MkTupleEdit EditFirst (KeyInsertReplaceItem item)) mr = do
            (isJust -> r2) <- lift $ mr $ MkTupleEditReader EditSecond $ KeyReadItem item ReadWhole
            return $
                if r2
                    then [KeyInsertReplaceItem item]
                    else []
        efUpdate (MkTupleEdit EditFirst KeyClear) mr = do
            keys1 <- lift $ mr $ MkTupleEditReader EditFirst KeyReadKeys
            keys2 <- lift $ mr $ MkTupleEditReader EditSecond KeyReadKeys
            return $
                if null $ keys1 /\ keys2
                    then []
                    else [KeyClear]
        efUpdate (MkTupleEdit EditSecond (KeyEditItem _ edit)) _ = never edit
        efUpdate (MkTupleEdit EditSecond (KeyDeleteItem item)) mr = do
            (isJust -> r1) <- lift $ mr $ MkTupleEditReader EditFirst $ KeyReadItem item ReadWhole
            return $
                if r1
                    then [KeyDeleteItem item]
                    else []
        efUpdate (MkTupleEdit EditSecond (KeyInsertReplaceItem item)) mr = do
            (isJust -> r1) <- lift $ mr $ MkTupleEditReader EditFirst $ KeyReadItem item ReadWhole
            return $
                if r1
                    then [KeyInsertReplaceItem item]
                    else []
        efUpdate (MkTupleEdit EditSecond KeyClear) mr = do
            keys2 <- lift $ mr $ MkTupleEditReader EditSecond KeyReadKeys
            keys1 <- lift $ mr $ MkTupleEditReader EditFirst KeyReadKeys
            return $
                if null $ keys2 /\ keys1
                    then []
                    else [KeyClear]
        in MkCloseUnlift identityUnlift MkAnEditFunction {..}
