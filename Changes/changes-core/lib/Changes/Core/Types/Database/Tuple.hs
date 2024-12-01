module Changes.Core.Types.Database.Tuple where

import Changes.Core.Import
import Changes.Core.Types.Database

data TupleTableSel tablesel row where
    MkTupleTableSel :: tablesel colsel -> TupleTableSel tablesel (AllOf colsel)

class TupleDatabaseType (dbType :: Type) where
    type TupleDatabaseTypeRowWitness dbType :: (Type -> Type) -> Constraint
    type TupleExpr dbType (colsel :: Type -> Type) :: Type -> Type
    evalTupleExpr :: Applicative m => TupleExpr dbType colsel t -> AllFor m colsel -> m t
    constBoolExpr :: Bool -> TupleExpr dbType colsel Bool
    columnExpr :: colsel t -> TupleExpr dbType colsel t

class TupleDatabaseType dbType => TupleDatabase dbType (tablesel :: (Type -> Type) -> Type) where
    type TupleDatabaseRowWitness dbType (tablesel :: (Type -> Type) -> Type) :: (Type -> Type) -> Constraint

evalTupleExprIdentity ::
       forall dbType colsel t. TupleDatabaseType dbType
    => TupleExpr dbType colsel t
    -> AllOf colsel
    -> t
evalTupleExprIdentity expr tuple = runIdentity $ evalTupleExpr @dbType expr $ allOfToAllFor tuple

type TupleDatabaseReader dbType tablesel = DatabaseReader dbType (TupleTableSel tablesel)

type TupleDatabaseEdit dbType tablesel = DatabaseEdit dbType (TupleTableSel tablesel)

data TupleWhereClause dbType row where
    MkTupleWhereClause :: TupleExpr dbType colsel Bool -> TupleWhereClause dbType (AllOf colsel)

data TupleUpdateItem dbType colsel where
    MkTupleUpdateItem :: colsel t -> TupleExpr dbType colsel t -> TupleUpdateItem dbType colsel

data TupleUpdateClause dbType row where
    MkTupleUpdateClause
        :: TestEquality colsel => [TupleUpdateItem dbType colsel] -> TupleUpdateClause dbType (AllOf colsel)

data TupleJoinClause rowa rowb rowc where
    OuterTupleJoinClause :: TupleJoinClause (AllOf colsel1) (AllOf colsel2) (AllOf (EitherType colsel1 colsel2))

instance TestEquality tablesel => TestEquality (TupleTableSel tablesel) where
    testEquality (MkTupleTableSel selTable1) (MkTupleTableSel selTable2) = do
        Refl <- testEquality selTable1 selTable2
        return Refl

data TupleSelectClause dbType tablesel row t where
    MkTupleSelectClause
        :: (TupleDatabaseTypeRowWitness dbType colsel', TupleDatabaseRowWitness dbType tablesel colsel')
        => (forall t. colsel' t -> TupleExpr dbType colsel t)
        -> TupleSelectClause dbType tablesel (AllOf colsel) (AllOf colsel')

data SortDir
    = SortAsc
    | SortDesc
    deriving stock (Eq)

instance Show SortDir where
    show SortAsc = "ASC"
    show SortDesc = "DESC"

data TupleOrderItem colsel where
    MkTupleOrderItem :: Ord t => colsel t -> SortDir -> TupleOrderItem colsel

data TupleOrderClause row where
    MkTupleOrderClause :: [TupleOrderItem colsel] -> TupleOrderClause (AllOf colsel)

instance Semigroup (TupleOrderClause (AllOf colsel)) where
    (MkTupleOrderClause item1) <> (MkTupleOrderClause item2) = MkTupleOrderClause $ item1 <> item2

instance Monoid (TupleOrderClause (AllOf colsel)) where
    mempty = MkTupleOrderClause mempty
    mappend = (<>)

data TupleInsertClause row where
    MkTupleInsertClause :: [AllOf colsel] -> TupleInsertClause (AllOf colsel)

instance ( WitnessConstraint (TupleDatabaseTypeRowWitness dbType) tablesel
         , WitnessConstraint (TupleDatabaseRowWitness dbType tablesel) tablesel
         , TupleDatabaseType dbType
         , TestEquality tablesel
         , FiniteWitness tablesel
         ) => Database dbType (TupleTableSel tablesel) where
    tableAssemble getrow = let
        conv :: AllFor (Compose f AllOf) tablesel -> AllFor f (TupleTableSel tablesel)
        conv (MkAllFor tcfa) = MkAllFor $ \(MkTupleTableSel tc) -> getCompose $ tcfa tc
        in fmap conv $ assembleAllFor $ \col -> fmap Compose $ getrow $ MkTupleTableSel col
    type WhereClause dbType (TupleTableSel tablesel) row = TupleWhereClause dbType row
    whereClause (MkTupleWhereClause expr) = evalTupleExprIdentity @dbType expr
    whereAlways (MkTupleTableSel (_ :: tablesel colsel)) = MkTupleWhereClause $ constBoolExpr @dbType @colsel True
    type InsertClause dbType (TupleTableSel tablesel) row = TupleInsertClause row
    insertClause (MkTupleInsertClause rows) = rows
    insertIntoTable (MkTupleTableSel _) = MkTupleInsertClause
    type UpdateClause dbType (TupleTableSel tablesel) row = TupleUpdateClause dbType row
    updateClause (MkTupleUpdateClause items) = let
        updateItem ::
               forall colsel. TestEquality colsel
            => TupleUpdateItem dbType colsel
            -> AllOf colsel
            -> AllOf colsel
        updateItem (MkTupleUpdateItem tsel expr) tuple@(MkAllOf tf) =
            MkAllOf $ \col ->
                case testEquality col tsel of
                    Just Refl -> evalTupleExprIdentity @dbType expr tuple
                    Nothing -> tf col
        updateItems [] = id
        updateItems (i:ii) = updateItems ii . updateItem i
        in updateItems items
    type OrderClause dbType (TupleTableSel tablesel) row = TupleOrderClause row
    orderClause (MkTupleOrderClause clauses) (MkAllOf tup1) (MkAllOf tup2) = let
        oc (MkTupleOrderItem colsel SortAsc) = compare (tup1 colsel) (tup2 colsel)
        oc (MkTupleOrderItem colsel SortDesc) = compare (Down $ tup1 colsel) (Down $ tup2 colsel)
        in concatmap oc clauses
    orderMonoid (MkTupleTableSel _) = Dict
    type SelectClause dbType (TupleTableSel tablesel) = TupleSelectClause dbType tablesel
    selectClause (MkTupleSelectClause selexpr) tuple =
        MkAllOf $ \col -> evalTupleExprIdentity @dbType (selexpr col) tuple
    selectRow (MkTupleTableSel tsel) =
        case witnessConstraint @_ @(TupleDatabaseTypeRowWitness dbType) tsel of
            Dict ->
                case witnessConstraint @_ @(TupleDatabaseRowWitness dbType tablesel) tsel of
                    Dict -> MkTupleSelectClause $ columnExpr @dbType
    type JoinClause dbType (TupleTableSel tablesel) = TupleJoinClause
    joinClause OuterTupleJoinClause = eitherAllOf
