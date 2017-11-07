module Truth.Core.Types.Database where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read

class TestEquality tablesel =>
      Database (database :: *) (tablesel :: * -> *) where
    tableAssemble :: Applicative m => (forall row. tablesel row -> m (f row)) -> m (AllF tablesel f)
    -- where
    type WhereClause database tablesel row :: *
    whereClause :: WhereClause database tablesel row -> row -> Bool
    whereAlways :: tablesel row -> WhereClause database tablesel row
    -- insert
    type InsertClause database tablesel row :: *
    insertClause :: InsertClause database tablesel row -> [row]
    insertIntoTable :: tablesel row -> [row] -> InsertClause database tablesel row
    -- update
    type UpdateClause database tablesel row :: *
    updateClause :: UpdateClause database tablesel row -> row -> row
    -- order
    type OrderClause database tablesel row :: *
    orderClause :: OrderClause database tablesel row -> row -> row -> Ordering
    orderMonoid :: tablesel row -> Dict (Monoid (OrderClause database tablesel row))
    -- select
    type SelectClause database tablesel :: * -> * -> *
    selectClause :: SelectClause database tablesel rowA rowB -> rowA -> rowB
    selectRow :: tablesel row -> SelectClause database tablesel row row
    -- join
    type JoinClause database tablesel :: * -> * -> * -> *
    joinClause :: JoinClause database tablesel rowA rowB rowC -> rowA -> rowB -> rowC -- outer joins only?

data TableJoin database tablesel row where
    SingleTable :: tablesel row -> TableJoin database tablesel row
    JoinTables
        :: JoinClause database tablesel rowA rowB rowC
        -> TableJoin database tablesel rowA
        -> TableJoin database tablesel rowB
        -> TableJoin database tablesel rowC

data DatabaseRead database tablesel t where
    DatabaseSelect
        :: TableJoin database tablesel row
        -> WhereClause database tablesel row
        -> OrderClause database tablesel row
        -> SelectClause database tablesel row row'
        -> DatabaseRead database tablesel [row']

instance Database database tablesel => SubjectReader (DatabaseRead database tablesel) where
    type ReaderSubject (DatabaseRead database tablesel) = AllF tablesel []
    readFromSubject (MkAllF tables) (DatabaseSelect j wc oc sc) = let
        doJoin :: TableJoin database tablesel row -> [row]
        doJoin (SingleTable tsel) = tables tsel
        doJoin (JoinTables jc j1 j2) = do
            row1 <- doJoin j1
            row2 <- doJoin j2
            return $ joinClause @database @tablesel jc row1 row2
        in fmap (selectClause @database @tablesel sc) $
           sortBy (orderClause @database @tablesel oc) $ filter (whereClause @database @tablesel wc) $ doJoin j

instance Database database tablesel => FullSubjectReader (DatabaseRead database tablesel) where
    subjectFromReader =
        tableAssemble @database $ \(tsel :: tablesel row) -> do
            Dict <- return $ orderMonoid @database tsel
            readable $ DatabaseSelect (SingleTable tsel) (whereAlways @database tsel) mempty (selectRow @database tsel)

data DatabaseEdit database tablesel where
    DatabaseInsert :: tablesel row -> InsertClause database tablesel row -> DatabaseEdit database tablesel
    DatabaseDelete :: tablesel row -> WhereClause database tablesel row -> DatabaseEdit database tablesel
    DatabaseUpdate
        :: tablesel row
        -> WhereClause database tablesel row
        -> UpdateClause database tablesel row
        -> DatabaseEdit database tablesel

instance Floating (DatabaseEdit database tablesel) (DatabaseEdit database tablesel)

instance Database database tablesel => Edit (DatabaseEdit database tablesel) where
    type EditReader (DatabaseEdit database tablesel) = DatabaseRead database tablesel
    applyEdit _ _ = return $ error "NYI: DatabaseEdit.applyEdit"
