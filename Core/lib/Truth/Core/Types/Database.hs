module Truth.Core.Types.Database where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;


    class TestEquality tablesel => Database (tablesel :: * -> *) where
    {
        tableAssemble :: Applicative m => (forall row. tablesel row -> m (f row)) -> m (DatabaseSubject f tablesel);

        type WhereClause tablesel :: * -> *;
        whereClause :: WhereClause tablesel row -> row -> Bool;
        whereMonoid :: ConstraintWitness (Monoid (WhereClause tablesel row));

        type InsertClause tablesel :: * -> *;
        insertClause :: InsertClause tablesel row -> [row];
        insertIntoTable :: tablesel row -> [row] -> InsertClause tablesel row;

        type UpdateClause tablesel :: * -> *;
        updateClause :: UpdateClause tablesel row -> row -> row;

        type OrderClause tablesel :: * -> *;
        orderClause :: OrderClause tablesel row -> row -> row -> Ordering;
        orderMonoid :: ConstraintWitness (Monoid (OrderClause tablesel row));

        type SelectClause tablesel :: * -> * -> *;
        selectClause :: SelectClause tablesel rowA rowB -> rowA -> rowB;
        selectRow :: tablesel row -> SelectClause tablesel row row;

        type JoinClause tablesel :: * -> * -> * -> *;
        joinClause :: JoinClause tablesel rowA rowB rowC -> rowA -> rowB -> rowC; -- outer joins only?
    };

    newtype DatabaseSubject f tablesel = MkDatabaseSubject (forall row. tablesel row -> f row);

    data Join tablesel row where
    {
        SingleTable :: tablesel row -> Join tablesel row;
        JoinTables :: JoinClause tablesel rowA rowB rowC -> Join tablesel rowA -> Join tablesel rowB -> Join tablesel rowC;
    };

    data DatabaseRead tablesel t where
    {
        DatabaseSelect :: Join tablesel row -> WhereClause tablesel row -> OrderClause tablesel row -> SelectClause tablesel row row' -> DatabaseRead tablesel [row'];
    };

    instance Database tablesel => Reader (DatabaseRead tablesel) where
    {
        type ReaderSubject (DatabaseRead tablesel) = DatabaseSubject [] tablesel;
        readFrom (MkDatabaseSubject tables) (DatabaseSelect j wc oc sc) = let
        {
            doJoin :: Join tablesel row -> [row];
            doJoin (SingleTable tsel) = tables tsel;
            doJoin (JoinTables jc j1 j2) = do
            {
                row1 <- doJoin j1;
                row2 <- doJoin j2;
                return $ joinClause @tablesel jc row1 row2;
            };
        } in fmap (selectClause @tablesel sc) $ sortBy (orderClause @tablesel oc) $ filter (whereClause @tablesel wc) $ doJoin j;
    };

    instance Database tablesel => FullReader c (DatabaseRead tablesel) where
    {
        fromReader = tableAssemble $ \(tsel :: tablesel row) -> do
        {
            MkConstraintWitness <- return $ whereMonoid @tablesel @row;
            MkConstraintWitness <- return $ orderMonoid @tablesel @row;
            readable $ DatabaseSelect (SingleTable tsel) mempty mempty (selectRow tsel);
        };
    };

    data DatabaseEdit tablesel where
    {
        DatabaseInsert :: tablesel row -> InsertClause tablesel row -> DatabaseEdit tablesel;
        DatabaseDelete :: tablesel row -> WhereClause tablesel row -> DatabaseEdit tablesel;
        DatabaseUpdate :: tablesel row -> WhereClause tablesel row -> UpdateClause tablesel row -> DatabaseEdit tablesel;
    };

    instance Floating (DatabaseEdit tablesel) (DatabaseEdit tablesel);

    instance Database tablesel => Edit (DatabaseEdit tablesel) where
    {
        type EditReader (DatabaseEdit tablesel) = DatabaseRead tablesel;
        applyEdit _ _ = return $ error "NYI: DatabaseEdit.applyEdit";
        invertEdit _ = return $ error "NYI: DatabaseEdit.invertEdit";
        {-
        invertEdit (DatabaseDelete (tsel :: tablesel row) wc) = do
        {
            MkConstraintWitness <- return $ orderMonoid @tablesel @row;
            rows <- readable $ DatabaseSelect (SingleTable tsel) wc mempty (selectRow tsel);
            return [DatabaseInsert tsel $ insertIntoTable tsel rows];
        };
        -}
    };
}
