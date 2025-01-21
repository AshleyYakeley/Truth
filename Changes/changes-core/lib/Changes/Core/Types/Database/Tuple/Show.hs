{-# OPTIONS -fno-warn-orphans #-}

module Changes.Core.Types.Database.Tuple.Show where

import Changes.Core.Import
import Changes.Core.Types.Database.Show
import Changes.Core.Types.Database.Tuple

instance AllConstraint Show tablesel => Show (TupleTableSel tablesel row) where
    show (MkTupleTableSel col) = allShow col

instance AllConstraint Show tablesel => AllConstraint Show (TupleTableSel tablesel) where
    allConstraint = Dict

class TupleDatabaseType dbType => ShowableTupleDatabaseType dbType where
    witnessShowTupleExpr ::
        forall colsel.
        (AllConstraint Show colsel, WitnessConstraint Show colsel) =>
        Dict (AllConstraint Show (TupleExpr dbType colsel))

class
    ( WitnessConstraint (TupleDatabaseRowWitness dbType tablesel) tablesel
    , ShowableTupleDatabaseType dbType
    , TestEquality tablesel
    , FiniteWitness tablesel
    , AllConstraint Show tablesel
    , WitnessConstraint FiniteWitness tablesel
    , WitnessConstraint (AllConstraint Show) tablesel
    , WitnessConstraint (WitnessConstraint Show) tablesel
    , WitnessConstraint (TupleDatabaseTypeRowWitness dbType) tablesel
    ) =>
    ShowableTupleDatabase dbType tablesel
    where
    witnessTupleRow ::
        forall colsel.
        TupleDatabaseRowWitness dbType tablesel colsel =>
        Dict (FiniteWitness colsel, WitnessConstraint Show colsel, AllConstraint Show colsel)

instance
    ( ShowableTupleDatabaseType dbType
    , row ~ AllOf colsel
    , AllConstraint Show colsel
    , WitnessConstraint Show colsel
    ) =>
    Show (TupleWhereClause dbType row)
    where
    show (MkTupleWhereClause expr) =
        case witnessShowTupleExpr @dbType @colsel of
            Dict -> allShow expr

instance
    (AllConstraint Show colsel, AllConstraint Show (TupleExpr dbType colsel)) =>
    Show (TupleUpdateItem dbType colsel)
    where
    show (MkTupleUpdateItem item expr) = allShow item ++ " " ++ allShow expr

instance
    ( ShowableTupleDatabaseType dbType
    , row ~ AllOf colsel
    , AllConstraint Show colsel
    , WitnessConstraint Show colsel
    ) =>
    Show (TupleUpdateClause dbType row)
    where
    show (MkTupleUpdateClause uitems) =
        case witnessShowTupleExpr @dbType @colsel of
            Dict -> show uitems

instance
    ( ShowableTupleDatabaseType dbType
    , AllConstraint Show colsel
    , WitnessConstraint Show colsel
    , FiniteWitness colsel'
    , AllConstraint Show colsel'
    ) =>
    Show (TupleSelectClause dbType tablesel (AllOf colsel) (AllOf colsel'))
    where
    show (MkTupleSelectClause ft) = let
        showItem :: Some colsel' -> String
        showItem (MkSome col) =
            allShow col
                ++ " -> "
                ++ case witnessShowTupleExpr @dbType @colsel of
                    Dict -> allShow (ft col)
        in "{" ++ intercalate "," (fmap showItem allWitnesses) ++ "}"

instance AllConstraint Show colsel => Show (TupleOrderItem colsel) where
    show (MkTupleOrderItem col dir) = allShow col ++ show dir

instance (row ~ AllOf colsel, AllConstraint Show colsel) => Show (TupleOrderClause row) where
    show (MkTupleOrderClause items) = show items

instance
    (row ~ AllOf colsel, FiniteWitness colsel, AllConstraint Show colsel, WitnessConstraint Show colsel) =>
    Show (TupleInsertClause row)
    where
    show (MkTupleInsertClause ics) = show ics

instance ShowableTupleDatabase dbType tablesel => ShowableDatabase dbType (TupleTableSel tablesel) where
    type
        ShowableRow dbType (TupleTableSel tablesel) row =
            ( row ~ (AllOf (UnAllOf row))
            , FiniteWitness (UnAllOf row)
            , WitnessConstraint Show (UnAllOf row)
            , AllConstraint Show (UnAllOf row)
            )
    showableRow = Dict
    showableTable (MkTupleTableSel tsel) =
        case witnessConstraint @_ @FiniteWitness tsel of
            Dict ->
                case witnessConstraint @_ @(WitnessConstraint Show) tsel of
                    Dict ->
                        case witnessConstraint @_ @(AllConstraint Show) tsel of
                            Dict -> Dict
    showableJoinClause OuterTupleJoinClause = Dict
    showableSelectClause sc@(MkTupleSelectClause _) = let
        dict ::
            forall colsel colsel'.
            TupleDatabaseRowWitness dbType tablesel colsel' =>
            TupleSelectClause dbType tablesel (AllOf colsel) (AllOf colsel') ->
            Dict (ShowableRow dbType (TupleTableSel tablesel) (AllOf colsel'))
        dict _ =
            case witnessTupleRow @dbType @tablesel @colsel' of
                Dict -> Dict
        in dict sc
    showJoinClause OuterTupleJoinClause = ","
    showSelectClause sc@(MkTupleSelectClause _) =
        case showableSelectClause @dbType @(TupleTableSel tablesel) sc of
            Dict -> show sc
    showWhereClause wc@(MkTupleWhereClause _) = show wc
    showOrderClause oc@(MkTupleOrderClause _) = show oc
    showInsertClause ic@(MkTupleInsertClause _) = show ic
    showUpdateClause uc@(MkTupleUpdateClause _) = show uc
