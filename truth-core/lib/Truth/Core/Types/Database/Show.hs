{-# OPTIONS -fno-warn-orphans #-}

module Truth.Core.Types.Database.Show where

import Truth.Core.Import
import Truth.Core.Types.Database

class (Database dbType tablesel, AllWitnessConstraint Show tablesel) =>
          ShowableDatabase (dbType :: Type) (tablesel :: Type -> Type) where
    type ShowableRow dbType tablesel row :: Constraint
    showableRow :: ShowableRow dbType tablesel row => Dict (Show row)
    showableTable :: forall row. tablesel row -> Dict (ShowableRow dbType tablesel row)
    showableJoinClause ::
           forall rowA rowB rowC. (ShowableRow dbType tablesel rowA, ShowableRow dbType tablesel rowB)
        => JoinClause dbType tablesel rowA rowB rowC
        -> Dict (ShowableRow dbType tablesel rowC)
    showableSelectClause ::
           forall rowA rowB. (ShowableRow dbType tablesel rowA)
        => SelectClause dbType tablesel rowA rowB
        -> Dict (ShowableRow dbType tablesel rowB)
    showJoinClause ::
           forall rowA rowB rowC. (ShowableRow dbType tablesel rowA, ShowableRow dbType tablesel rowB)
        => JoinClause dbType tablesel rowA rowB rowC
        -> String
    showSelectClause ::
           forall row row'. ShowableRow dbType tablesel row
        => SelectClause dbType tablesel row row'
        -> String
    showWhereClause ::
           forall row. ShowableRow dbType tablesel row
        => WhereClause dbType tablesel row
        -> String
    showOrderClause ::
           forall row. ShowableRow dbType tablesel row
        => OrderClause dbType tablesel row
        -> String
    showInsertClause ::
           forall row. ShowableRow dbType tablesel row
        => InsertClause dbType tablesel row
        -> String
    showUpdateClause ::
           forall row. ShowableRow dbType tablesel row
        => UpdateClause dbType tablesel row
        -> String

showableTableJoin ::
       forall dbType tablesel row. ShowableDatabase dbType tablesel
    => TableJoin dbType tablesel row
    -> Dict (ShowableRow dbType tablesel row)
showableTableJoin (SingleTable tsel) =
    case showableTable @dbType @tablesel tsel of
        Dict -> Dict
showableTableJoin (JoinTables jc tj1 tj2) =
    case (showableTableJoin tj1, showableTableJoin tj2) of
        (Dict, Dict) -> showableJoinClause @dbType @tablesel jc

instance ShowableDatabase dbType tablesel => Show (TableJoin dbType tablesel row) where
    show (SingleTable table) = showAllWitness table
    show (JoinTables j t1 t2) =
        case (showableTableJoin t1, showableTableJoin t2) of
            (Dict, Dict) -> "(" ++ show t1 ++ " " ++ showJoinClause @dbType @tablesel j ++ " " ++ show t2 ++ ")"

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

instance ShowableDatabase dbType tablesel => Show (DatabaseRead dbType tablesel t) where
    show (DatabaseSelect (tjoin :: TableJoin dbType tablesel row) wc oc sc) =
        case showableTableJoin tjoin of
            Dict ->
                "select " ++
                showSelectClause @dbType @tablesel sc ++
                " from " ++
                show tjoin ++
                " where " ++
                showWhereClause @dbType @tablesel @row wc ++ " order by " ++ showOrderClause @dbType @tablesel @row oc

instance ShowableDatabase dbType tablesel => Show (DatabaseEdit dbType tablesel) where
    show (DatabaseInsert (row :: tablesel row) ic) =
        case showableTable @dbType @tablesel row of
            Dict -> "insert " ++ showAllWitness row ++ " " ++ showInsertClause @dbType @tablesel @row ic
    show (DatabaseDelete (row :: tablesel row) wc) =
        "delete " ++
        showAllWitness row ++
        " " ++
        case showableTable @dbType @tablesel row of
            Dict -> showWhereClause @dbType @tablesel @row wc
    show (DatabaseUpdate (row :: tablesel row) wc uc) =
        case showableTable @dbType @tablesel row of
            Dict ->
                "update " ++
                showAllWitness row ++
                " " ++ showWhereClause @dbType @tablesel @row wc ++ " " ++ showUpdateClause @dbType @tablesel @row uc
