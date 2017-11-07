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

type FiniteSetReader subj = KeyReader (FiniteSet subj) (WholeReader subj)

type FiniteSetEdit subj = KeyEdit (FiniteSet subj) (ConstEdit subj)

finiteSetEditLens ::
       forall subj. Eq subj
    => subj
    -> PureEditLens (FiniteSetEdit subj) (WholeEdit Bool)
finiteSetEditLens subj = let
    editAccess :: IOStateAccess ()
    editAccess = unitStateAccess
    editGet :: () -> ReadFunction (FiniteSetReader subj) (WholeReader Bool)
    editGet () ReadWhole = fmap isJust $ readable $ KeyReadItem subj ReadWhole
    editUpdate :: FiniteSetEdit subj -> () -> Readable (FiniteSetReader subj) ((), [WholeEdit Bool])
    editUpdate (KeyEditItem _ edit) () = never edit
    editUpdate (KeyDeleteItem key) ()
        | key == subj = return $ pure [MkWholeEdit False]
    editUpdate (KeyDeleteItem _) () = return $ pure []
    editUpdate (KeyInsertReplaceItem key) ()
        | key == subj = return $ pure [MkWholeEdit True]
    editUpdate (KeyInsertReplaceItem _) () = return $ pure []
    editUpdate KeyClear () = return $ pure [MkWholeEdit False]
    editLensFunction = MkEditFunction {..}
    editLensPutEdit :: () -> WholeEdit Bool -> Readable (FiniteSetReader subj) (Maybe ((), [FiniteSetEdit subj]))
    editLensPutEdit () (MkWholeEdit False) = return $ pure $ pure $ [KeyDeleteItem subj]
    editLensPutEdit () (MkWholeEdit True) = return $ pure $ pure $ [KeyInsertReplaceItem subj]
    in MkEditLens {..}

instance Eq subj => JoinSemiLatticeEdit (FiniteSetEdit subj) where
    joinEditFunction = let
        editAccess :: IOStateAccess ()
        editAccess = unitStateAccess
        editGet :: () -> ReadFunction (PairEditReader (FiniteSetEdit subj) (FiniteSetEdit subj)) (FiniteSetReader subj)
        editGet () KeyReadKeys = do
            keys1 <- readable $ MkTupleEditReader EditFirst KeyReadKeys
            keys2 <- readable $ MkTupleEditReader EditSecond KeyReadKeys
            return $ keys1 \/ keys2
        editGet () (KeyReadItem item ReadWhole) = do
            (isJust -> r1) <- readable $ MkTupleEditReader EditFirst $ KeyReadItem item ReadWhole
            (isJust -> r2) <- readable $ MkTupleEditReader EditSecond $ KeyReadItem item ReadWhole
            return $
                if r1 \/ r2
                    then Just item
                    else Nothing
        editUpdate ::
               PairEdit (FiniteSetEdit subj) (FiniteSetEdit subj)
            -> ()
            -> Readable (PairEditReader (FiniteSetEdit subj) (FiniteSetEdit subj)) ((), [FiniteSetEdit subj])
        editUpdate (MkTupleEdit EditFirst (KeyEditItem _ edit)) () = never edit
        editUpdate (MkTupleEdit EditFirst (KeyDeleteItem item)) () = do
            (isJust -> r2) <- readable $ MkTupleEditReader EditSecond $ KeyReadItem item ReadWhole
            return $
                pure $
                if r2
                    then []
                    else [KeyDeleteItem item]
        editUpdate (MkTupleEdit EditFirst (KeyInsertReplaceItem item)) () = do
            (isJust -> r2) <- readable $ MkTupleEditReader EditSecond $ KeyReadItem item ReadWhole
            return $
                pure $
                if r2
                    then []
                    else [KeyInsertReplaceItem item]
        editUpdate (MkTupleEdit EditFirst KeyClear) () = do
            keys1 <- readable $ MkTupleEditReader EditFirst KeyReadKeys
            keys2 <- readable $ MkTupleEditReader EditSecond KeyReadKeys
            return $
                pure $
                case (null keys1, null keys2) of
                    (True, _) -> []
                    (False, True) -> [KeyClear]
                    (False, False) -> fmap KeyDeleteItem $ toList $ difference keys1 keys2
        editUpdate (MkTupleEdit EditSecond (KeyEditItem _ edit)) () = never edit
        editUpdate (MkTupleEdit EditSecond (KeyDeleteItem item)) () = do
            (isJust -> r1) <- readable $ MkTupleEditReader EditFirst $ KeyReadItem item ReadWhole
            return $
                pure $
                if r1
                    then []
                    else [KeyDeleteItem item]
        editUpdate (MkTupleEdit EditSecond (KeyInsertReplaceItem item)) () = do
            (isJust -> r1) <- readable $ MkTupleEditReader EditFirst $ KeyReadItem item ReadWhole
            return $
                pure $
                if r1
                    then []
                    else [KeyInsertReplaceItem item]
        editUpdate (MkTupleEdit EditSecond KeyClear) () = do
            keys2 <- readable $ MkTupleEditReader EditSecond KeyReadKeys
            keys1 <- readable $ MkTupleEditReader EditFirst KeyReadKeys
            return $
                pure $
                case (null keys2, null keys1) of
                    (True, _) -> []
                    (False, True) -> [KeyClear]
                    (False, False) -> fmap KeyDeleteItem $ toList $ difference keys2 keys1
        in MkEditFunction {..}

instance Eq subj => MeetSemiLatticeEdit (FiniteSetEdit subj) where
    meetEditFunction = let
        editAccess :: IOStateAccess ()
        editAccess = unitStateAccess
        editGet :: () -> ReadFunction (PairEditReader (FiniteSetEdit subj) (FiniteSetEdit subj)) (FiniteSetReader subj)
        editGet () KeyReadKeys = do
            keys1 <- readable $ MkTupleEditReader EditFirst KeyReadKeys
            keys2 <- readable $ MkTupleEditReader EditSecond KeyReadKeys
            return $ keys1 /\ keys2
        editGet () (KeyReadItem item ReadWhole) = do
            (isJust -> r1) <- readable $ MkTupleEditReader EditFirst $ KeyReadItem item ReadWhole
            (isJust -> r2) <- readable $ MkTupleEditReader EditSecond $ KeyReadItem item ReadWhole
            return $
                if r1 /\ r2
                    then Just item
                    else Nothing
        editUpdate ::
               PairEdit (FiniteSetEdit subj) (FiniteSetEdit subj)
            -> ()
            -> Readable (PairEditReader (FiniteSetEdit subj) (FiniteSetEdit subj)) ((), [FiniteSetEdit subj])
        editUpdate (MkTupleEdit EditFirst (KeyEditItem _ edit)) () = never edit
        editUpdate (MkTupleEdit EditFirst (KeyDeleteItem item)) () = do
            (isJust -> r2) <- readable $ MkTupleEditReader EditSecond $ KeyReadItem item ReadWhole
            return $
                pure $
                if r2
                    then [KeyDeleteItem item]
                    else []
        editUpdate (MkTupleEdit EditFirst (KeyInsertReplaceItem item)) () = do
            (isJust -> r2) <- readable $ MkTupleEditReader EditSecond $ KeyReadItem item ReadWhole
            return $
                pure $
                if r2
                    then [KeyInsertReplaceItem item]
                    else []
        editUpdate (MkTupleEdit EditFirst KeyClear) () = do
            keys1 <- readable $ MkTupleEditReader EditFirst KeyReadKeys
            keys2 <- readable $ MkTupleEditReader EditSecond KeyReadKeys
            return $
                pure $
                if null $ keys1 /\ keys2
                    then []
                    else [KeyClear]
        editUpdate (MkTupleEdit EditSecond (KeyEditItem _ edit)) () = never edit
        editUpdate (MkTupleEdit EditSecond (KeyDeleteItem item)) () = do
            (isJust -> r1) <- readable $ MkTupleEditReader EditFirst $ KeyReadItem item ReadWhole
            return $
                pure $
                if r1
                    then [KeyDeleteItem item]
                    else []
        editUpdate (MkTupleEdit EditSecond (KeyInsertReplaceItem item)) () = do
            (isJust -> r1) <- readable $ MkTupleEditReader EditFirst $ KeyReadItem item ReadWhole
            return $
                pure $
                if r1
                    then [KeyInsertReplaceItem item]
                    else []
        editUpdate (MkTupleEdit EditSecond KeyClear) () = do
            keys2 <- readable $ MkTupleEditReader EditSecond KeyReadKeys
            keys1 <- readable $ MkTupleEditReader EditFirst KeyReadKeys
            return $
                pure $
                if null $ keys2 /\ keys1
                    then []
                    else [KeyClear]
        in MkEditFunction {..}
