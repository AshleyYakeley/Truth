module Changes.Core.Types.Database where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Read

class TestEquality tablesel => Database (dbType :: Type) (tablesel :: Type -> Type) where
    tableAssemble :: Applicative m => (forall row. tablesel row -> m (f row)) -> m (AllFor f tablesel)

    -- where
    type WhereClause dbType tablesel row :: Type
    whereClause :: WhereClause dbType tablesel row -> row -> Bool
    whereAlways :: tablesel row -> WhereClause dbType tablesel row

    -- insert
    type InsertClause dbType tablesel row :: Type
    insertClause :: InsertClause dbType tablesel row -> [row]
    insertIntoTable :: tablesel row -> [row] -> InsertClause dbType tablesel row

    -- update
    type UpdateClause dbType tablesel row :: Type
    updateClause :: UpdateClause dbType tablesel row -> row -> row

    -- order
    type OrderClause dbType tablesel row :: Type
    orderClause :: OrderClause dbType tablesel row -> row -> row -> Ordering
    orderMonoid :: tablesel row -> Dict (Monoid (OrderClause dbType tablesel row))

    -- select
    type SelectClause dbType tablesel :: Type -> Type -> Type
    selectClause :: SelectClause dbType tablesel rowA rowB -> rowA -> rowB
    selectRow :: tablesel row -> SelectClause dbType tablesel row row

    -- join
    type JoinClause dbType tablesel :: Type -> Type -> Type -> Type
    joinClause :: JoinClause dbType tablesel rowA rowB rowC -> rowA -> rowB -> rowC -- outer joins only?

data TableJoin dbType tablesel row where
    SingleTable :: tablesel row -> TableJoin dbType tablesel row
    JoinTables ::
        JoinClause dbType tablesel rowA rowB rowC ->
        TableJoin dbType tablesel rowA ->
        TableJoin dbType tablesel rowB ->
        TableJoin dbType tablesel rowC

data DatabaseReader dbType tablesel t where
    DatabaseSelect ::
        TableJoin dbType tablesel row ->
        WhereClause dbType tablesel row ->
        OrderClause dbType tablesel row ->
        SelectClause dbType tablesel row row' ->
        DatabaseReader dbType tablesel [row']

instance Database dbType tablesel => SubjectReader (DatabaseReader dbType tablesel) where
    type ReaderSubject (DatabaseReader dbType tablesel) = AllFor [] tablesel
    subjectToRead (MkAllFor tables) (DatabaseSelect j wc oc sc) = let
        doJoin :: TableJoin dbType tablesel row -> [row]
        doJoin (SingleTable tsel) = tables tsel
        doJoin (JoinTables jc j1 j2) = do
            row1 <- doJoin j1
            row2 <- doJoin j2
            return $ joinClause @dbType @tablesel jc row1 row2
        in fmap (selectClause @dbType @tablesel sc)
            $ sortBy (orderClause @dbType @tablesel oc)
            $ filter (whereClause @dbType @tablesel wc)
            $ doJoin j

instance Database dbType tablesel => FullSubjectReader (DatabaseReader dbType tablesel) where
    readableToSubject mr =
        tableAssemble @dbType $ \(tsel :: tablesel row) -> do
            Dict <- return $ orderMonoid @dbType tsel
            mr $ DatabaseSelect (SingleTable tsel) (whereAlways @dbType tsel) mempty (selectRow @dbType tsel)

data DatabaseEdit dbType tablesel where
    DatabaseInsert :: tablesel row -> InsertClause dbType tablesel row -> DatabaseEdit dbType tablesel
    DatabaseDelete :: tablesel row -> WhereClause dbType tablesel row -> DatabaseEdit dbType tablesel
    DatabaseUpdate ::
        tablesel row ->
        WhereClause dbType tablesel row ->
        UpdateClause dbType tablesel row ->
        DatabaseEdit dbType tablesel

type instance EditReader (DatabaseEdit dbType tablesel) = DatabaseReader dbType tablesel
