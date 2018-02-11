{-# OPTIONS -fno-warn-orphans #-}

module Truth.Core.Types.Database.Show where

import Truth.Core.Import
import Truth.Core.Types.Database

class (Database database tablesel, AllWitnessConstraint Show tablesel) =>
      ShowableDatabase (database :: *) (tablesel :: * -> *) where
    type ShowableRow database tablesel row :: Constraint
    showableRow :: ShowableRow database tablesel row => Dict (Show row)
    showableTable :: forall row. tablesel row -> Dict (ShowableRow database tablesel row)
    showableJoinClause ::
           forall rowA rowB rowC. (ShowableRow database tablesel rowA, ShowableRow database tablesel rowB)
        => JoinClause database tablesel rowA rowB rowC
        -> Dict (ShowableRow database tablesel rowC)
    showableSelectClause ::
           forall rowA rowB. (ShowableRow database tablesel rowA)
        => SelectClause database tablesel rowA rowB
        -> Dict (ShowableRow database tablesel rowB)
    showJoinClause ::
           forall rowA rowB rowC. (ShowableRow database tablesel rowA, ShowableRow database tablesel rowB)
        => JoinClause database tablesel rowA rowB rowC
        -> String
    showSelectClause ::
           forall row row'. ShowableRow database tablesel row
        => SelectClause database tablesel row row'
        -> String
    showWhereClause ::
           forall row. ShowableRow database tablesel row
        => WhereClause database tablesel row
        -> String
    showOrderClause ::
           forall row. ShowableRow database tablesel row
        => OrderClause database tablesel row
        -> String
    showInsertClause ::
           forall row. ShowableRow database tablesel row
        => InsertClause database tablesel row
        -> String
    showUpdateClause ::
           forall row. ShowableRow database tablesel row
        => UpdateClause database tablesel row
        -> String

showableTableJoin ::
       forall database tablesel row. ShowableDatabase database tablesel
    => TableJoin database tablesel row
    -> Dict (ShowableRow database tablesel row)
showableTableJoin (SingleTable tsel) =
    case showableTable @database @tablesel tsel of
        Dict -> Dict
showableTableJoin (JoinTables jc tj1 tj2) =
    case (showableTableJoin tj1, showableTableJoin tj2) of
        (Dict, Dict) -> showableJoinClause @database @tablesel jc

instance ShowableDatabase database tablesel => Show (TableJoin database tablesel row) where
    show (SingleTable table) = showAllWitness table
    show (JoinTables j t1 t2) =
        case (showableTableJoin t1, showableTableJoin t2) of
            (Dict, Dict) -> "(" ++ show t1 ++ " " ++ showJoinClause @database @tablesel j ++ " " ++ show t2 ++ ")"

instance ShowableDatabase dbtype tablesel => AllWitnessConstraint Show (DatabaseRead dbtype tablesel) where
    allWitnessConstraint = Dict

instance ShowableDatabase dbtype tablesel => WitnessConstraint Show (DatabaseRead dbtype tablesel) where
    witnessConstraint (DatabaseSelect tj _ _ (sc :: SelectClause dbtype tablesel row row')) =
        case showableTableJoin @dbtype @tablesel tj of
            Dict ->
                case showableSelectClause @dbtype @tablesel sc of
                    Dict ->
                        case showableRow @dbtype @tablesel @row' of
                            Dict -> Dict

instance ShowableDatabase database tablesel => Show (DatabaseRead database tablesel t) where
    show (DatabaseSelect (tjoin :: TableJoin database tablesel row) wc oc sc) =
        case showableTableJoin tjoin of
            Dict ->
                "select " ++
                showSelectClause @database @tablesel sc ++
                " from " ++
                show tjoin ++
                " where " ++
                showWhereClause @database @tablesel @row wc ++
                " order by " ++ showOrderClause @database @tablesel @row oc

instance ShowableDatabase database tablesel => Show (DatabaseEdit database tablesel) where
    show (DatabaseInsert (row :: tablesel row) ic) =
        case showableTable @database @tablesel row of
            Dict -> "insert " ++ showAllWitness row ++ " " ++ showInsertClause @database @tablesel @row ic
    show (DatabaseDelete (row :: tablesel row) wc) =
        "delete " ++
        showAllWitness row ++
        " " ++
        case showableTable @database @tablesel row of
            Dict -> showWhereClause @database @tablesel @row wc
    show (DatabaseUpdate (row :: tablesel row) wc uc) =
        case showableTable @database @tablesel row of
            Dict ->
                "update " ++
                showAllWitness row ++
                " " ++
                showWhereClause @database @tablesel @row wc ++ " " ++ showUpdateClause @database @tablesel @row uc
