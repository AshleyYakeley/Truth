module Truth.Core.Types.TupleDatabase where

import Truth.Core.Import
import Truth.Core.Types.Database

data TupleTableSel tablesel row where
    MkTupleTableSel :: tablesel colsel -> TupleTableSel tablesel (All colsel)

class TupleDatabase (database :: *) where
    type TupleDatabaseRowWitness database :: (* -> *) -> Constraint
    type TupleExpr database (colsel :: * -> *) :: * -> *
    evalTupleExpr :: Applicative m => TupleExpr database colsel t -> AllF colsel m -> m t
    constBoolExpr :: Bool -> TupleExpr database colsel Bool
    columnExpr :: colsel t -> TupleExpr database colsel t

evalTupleExprIdentity ::
       forall database colsel t. TupleDatabase database
    => TupleExpr database colsel t
    -> All colsel
    -> t
evalTupleExprIdentity expr tuple = runIdentity $ evalTupleExpr @database expr $ allToAllF tuple

type TupleDatabaseRead database tablesel = DatabaseRead database (TupleTableSel tablesel)

type TupleDatabaseEdit database tablesel = DatabaseEdit database (TupleTableSel tablesel)

data TupleWhereClause database row where
    MkTupleWhereClause :: TupleExpr database colsel Bool -> TupleWhereClause database (All colsel)

data TupleUpdateItem database colsel where
    MkTupleUpdateItem :: colsel t -> TupleExpr database colsel t -> TupleUpdateItem database colsel

data TupleUpdateClause database row where
    MkTupleUpdateClause
        :: TestEquality colsel => [TupleUpdateItem database colsel] -> TupleUpdateClause database (All colsel)

data TupleJoinClause rowa rowb rowc where
    OuterTupleJoinClause :: TupleJoinClause (All colsel1) (All colsel2) (All (EitherWitness colsel1 colsel2))

instance TestEquality tablesel => TestEquality (TupleTableSel tablesel) where
    testEquality (MkTupleTableSel selTable1) (MkTupleTableSel selTable2) = do
        Refl <- testEquality selTable1 selTable2
        return Refl

data TupleSelectClause database row t where
    MkTupleSelectClause
        :: TupleDatabaseRowWitness database colsel'
        => (forall t. colsel' t -> TupleExpr database colsel t)
        -> TupleSelectClause database (All colsel) (All colsel')

data SortDir
    = SortAsc
    | SortDesc
    deriving (Eq)

instance Show SortDir where
    show SortAsc = "ASC"
    show SortDesc = "DESC"

data TupleOrderItem colsel where
    MkTupleOrderItem :: Ord t => colsel t -> SortDir -> TupleOrderItem colsel

data TupleOrderClause row where
    MkTupleOrderClause :: [TupleOrderItem colsel] -> TupleOrderClause (All colsel)

instance Semigroup (TupleOrderClause (All colsel)) where
    (MkTupleOrderClause item1) <> (MkTupleOrderClause item2) = MkTupleOrderClause $ item1 <> item2

instance Monoid (TupleOrderClause (All colsel)) where
    mempty = MkTupleOrderClause mempty
    mappend = (<>)

data TupleInsertClause row where
    MkTupleInsertClause :: [All colsel] -> TupleInsertClause (All colsel)

instance ( WitnessConstraint (TupleDatabaseRowWitness database) tablesel
         , TupleDatabase database
         , TestEquality tablesel
         , FiniteWitness tablesel
         ) =>
         Database database (TupleTableSel tablesel) where
    tableAssemble getrow =
        let conv :: AllF tablesel (Compose f All) -> AllF (TupleTableSel tablesel) f
            conv (MkAllF tcfa) = MkAllF $ \(MkTupleTableSel tc) -> getCompose $ tcfa tc
        in fmap conv $ assembleWitnessF $ \col -> fmap Compose $ getrow $ MkTupleTableSel col
    type WhereClause database (TupleTableSel tablesel) row = TupleWhereClause database row
    whereClause (MkTupleWhereClause expr) = evalTupleExprIdentity @database expr
    whereAlways (MkTupleTableSel (_ :: tablesel colsel)) = MkTupleWhereClause $ constBoolExpr @database @colsel True
    type InsertClause database (TupleTableSel tablesel) row = TupleInsertClause row
    insertClause (MkTupleInsertClause rows) = rows
    insertIntoTable (MkTupleTableSel _) = MkTupleInsertClause
    type UpdateClause database (TupleTableSel tablesel) row = TupleUpdateClause database row
    updateClause (MkTupleUpdateClause items) =
        let updateItem ::
                   forall colsel. TestEquality colsel
                => TupleUpdateItem database colsel
                -> All colsel
                -> All colsel
            updateItem (MkTupleUpdateItem tsel expr) tuple@(MkAll tf) =
                MkAll $ \col ->
                    case testEquality col tsel of
                        Just Refl -> evalTupleExprIdentity @database expr tuple
                        Nothing -> tf col
            updateItems [] = id
            updateItems (i:ii) = updateItems ii . updateItem i
        in updateItems items
    type OrderClause database (TupleTableSel tablesel) row = TupleOrderClause row
    orderClause (MkTupleOrderClause clauses) (MkAll tup1) (MkAll tup2) =
        let oc (MkTupleOrderItem colsel SortAsc) = compare (tup1 colsel) (tup2 colsel)
            oc (MkTupleOrderItem colsel SortDesc) = compare (Down $ tup1 colsel) (Down $ tup2 colsel)
        in mconcat $ fmap oc clauses
    orderMonoid (MkTupleTableSel _) = Dict
    type SelectClause database (TupleTableSel tablesel) = TupleSelectClause database
    selectClause (MkTupleSelectClause selexpr) tuple =
        MkAll $ \col -> evalTupleExprIdentity @database (selexpr col) tuple
    selectRow (MkTupleTableSel tsel) =
        case witnessConstraint @_ @(TupleDatabaseRowWitness database) tsel of
            Dict -> MkTupleSelectClause $ columnExpr @database
    type JoinClause database (TupleTableSel tablesel) = TupleJoinClause
    joinClause OuterTupleJoinClause = eitherAll
