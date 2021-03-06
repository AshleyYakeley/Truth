{-# OPTIONS -fno-warn-orphans #-}

module Changes.Core.Types.Database.Tuple.Show where

import Changes.Core.Import
import Changes.Core.Types.Database.Show
import Changes.Core.Types.Database.Tuple

instance AllWitnessConstraint Show tablesel => Show (TupleTableSel tablesel row) where
    show (MkTupleTableSel col) = showAllWitness col

instance AllWitnessConstraint Show tablesel => AllWitnessConstraint Show (TupleTableSel tablesel) where
    allWitnessConstraint = Dict

class TupleDatabaseType dbType => ShowableTupleDatabaseType dbType where
    witnessShowTupleExpr ::
           forall colsel. (AllWitnessConstraint Show colsel, WitnessConstraint Show colsel)
        => Dict (AllWitnessConstraint Show (TupleExpr dbType colsel))

class ( WitnessConstraint (TupleDatabaseRowWitness dbType tablesel) tablesel
      , ShowableTupleDatabaseType dbType
      , TestEquality tablesel
      , FiniteWitness tablesel
      , AllWitnessConstraint Show tablesel
      , WitnessConstraint FiniteWitness tablesel
      , WitnessConstraint (AllWitnessConstraint Show) tablesel
      , WitnessConstraint (WitnessConstraint Show) tablesel
      , WitnessConstraint (TupleDatabaseTypeRowWitness dbType) tablesel
      ) => ShowableTupleDatabase dbType tablesel where
    witnessTupleRow ::
           forall colsel. TupleDatabaseRowWitness dbType tablesel colsel
        => Dict (FiniteWitness colsel, WitnessConstraint Show colsel, AllWitnessConstraint Show colsel)

instance ( ShowableTupleDatabaseType dbType
         , row ~ AllValue colsel
         , AllWitnessConstraint Show colsel
         , WitnessConstraint Show colsel
         ) => Show (TupleWhereClause dbType row) where
    show (MkTupleWhereClause expr) =
        case witnessShowTupleExpr @dbType @colsel of
            Dict -> showAllWitness expr

instance (AllWitnessConstraint Show colsel, AllWitnessConstraint Show (TupleExpr dbType colsel)) =>
             Show (TupleUpdateItem dbType colsel) where
    show (MkTupleUpdateItem item expr) = showAllWitness item ++ " " ++ showAllWitness expr

instance ( ShowableTupleDatabaseType dbType
         , row ~ AllValue colsel
         , AllWitnessConstraint Show colsel
         , WitnessConstraint Show colsel
         ) => Show (TupleUpdateClause dbType row) where
    show (MkTupleUpdateClause uitems) =
        case witnessShowTupleExpr @dbType @colsel of
            Dict -> show uitems

instance ( ShowableTupleDatabaseType dbType
         , AllWitnessConstraint Show colsel
         , WitnessConstraint Show colsel
         , FiniteWitness colsel'
         , AllWitnessConstraint Show colsel'
         ) => Show (TupleSelectClause dbType tablesel (AllValue colsel) (AllValue colsel')) where
    show (MkTupleSelectClause ft) = let
        showItem :: AnyW colsel' -> String
        showItem (MkAnyW col) =
            showAllWitness col ++
            " -> " ++
            case witnessShowTupleExpr @dbType @colsel of
                Dict -> showAllWitness (ft col)
        in "{" ++ intercalate "," (fmap showItem allWitnesses) ++ "}"

instance AllWitnessConstraint Show colsel => Show (TupleOrderItem colsel) where
    show (MkTupleOrderItem col dir) = showAllWitness col ++ show dir

instance (row ~ AllValue colsel, AllWitnessConstraint Show colsel) => Show (TupleOrderClause row) where
    show (MkTupleOrderClause items) = show items

instance (row ~ AllValue colsel, FiniteWitness colsel, AllWitnessConstraint Show colsel, WitnessConstraint Show colsel) =>
             Show (TupleInsertClause row) where
    show (MkTupleInsertClause ics) = show ics

instance ShowableTupleDatabase dbType tablesel => ShowableDatabase dbType (TupleTableSel tablesel) where
    type ShowableRow dbType (TupleTableSel tablesel) row = ( row ~ (AllValue (UnAllValue row))
                                                           , FiniteWitness (UnAllValue row)
                                                           , WitnessConstraint Show (UnAllValue row)
                                                           , AllWitnessConstraint Show (UnAllValue row))
    showableRow = Dict
    showableTable (MkTupleTableSel tsel) =
        case witnessConstraint @_ @FiniteWitness tsel of
            Dict ->
                case witnessConstraint @_ @(WitnessConstraint Show) tsel of
                    Dict ->
                        case witnessConstraint @_ @(AllWitnessConstraint Show) tsel of
                            Dict -> Dict
    showableJoinClause OuterTupleJoinClause = Dict
    showableSelectClause sc@(MkTupleSelectClause _) = let
        dict ::
               forall colsel colsel'. TupleDatabaseRowWitness dbType tablesel colsel'
            => TupleSelectClause dbType tablesel (AllValue colsel) (AllValue colsel')
            -> Dict (ShowableRow dbType (TupleTableSel tablesel) (AllValue colsel'))
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
