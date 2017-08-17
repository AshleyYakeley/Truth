module Truth.Core.Types.Database where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;


    newtype All (w :: k -> *) (f :: k -> *) = MkAll (forall (t :: k). w t -> f t);

    class TestEquality tablesel => Database (database :: *) (tablesel :: * -> *) where
    {
        tableAssemble :: Applicative m => (forall row. tablesel row -> m (f row)) -> m (All tablesel f);

        type WhereClause database tablesel :: * -> *;
        whereClause :: WhereClause database tablesel row -> row -> Bool;
        whereAlways :: tablesel row -> WhereClause database tablesel row;

        type InsertClause database tablesel :: * -> *;
        insertClause :: InsertClause database tablesel row -> [row];
        insertIntoTable :: tablesel row -> [row] -> InsertClause database tablesel row;

        type UpdateClause database tablesel :: * -> *;
        updateClause :: UpdateClause database tablesel row -> row -> row;

        type OrderClause database tablesel :: * -> *;
        orderClause :: OrderClause database tablesel row -> row -> row -> Ordering;
        orderMonoid :: tablesel row -> ConstraintWitness (Monoid (OrderClause database tablesel row));

        type SelectClause database tablesel :: * -> * -> *;
        selectClause :: SelectClause database tablesel rowA rowB -> rowA -> rowB;
        selectRow :: tablesel row -> SelectClause database tablesel row row;

        type JoinClause database tablesel :: * -> * -> * -> *;
        joinClause :: JoinClause database tablesel rowA rowB rowC -> rowA -> rowB -> rowC; -- outer joins only?
    };

    data Join database tablesel row where
    {
        SingleTable :: tablesel row -> Join database tablesel row;
        JoinTables :: JoinClause database tablesel rowA rowB rowC -> Join database tablesel rowA -> Join database tablesel rowB -> Join database tablesel rowC;
    };

    data DatabaseRead database tablesel t where
    {
        DatabaseSelect :: Join database tablesel row -> WhereClause database tablesel row -> OrderClause database tablesel row -> SelectClause database tablesel row row' -> DatabaseRead database tablesel [row'];
    };

    instance Database database tablesel => Reader (DatabaseRead database tablesel) where
    {
        type ReaderSubject (DatabaseRead database tablesel) = All tablesel [];
        readFrom (MkAll tables) (DatabaseSelect j wc oc sc) = let
        {
            doJoin :: Join database tablesel row -> [row];
            doJoin (SingleTable tsel) = tables tsel;
            doJoin (JoinTables jc j1 j2) = do
            {
                row1 <- doJoin j1;
                row2 <- doJoin j2;
                return $ joinClause @database @tablesel jc row1 row2;
            };
        } in fmap (selectClause @database @tablesel sc) $ sortBy (orderClause @database @tablesel oc) $ filter (whereClause @database @tablesel wc) $ doJoin j;
    };

    instance Database database tablesel => FullReader c (DatabaseRead database tablesel) where
    {
        fromReader = tableAssemble @database $ \(tsel :: tablesel row) -> do
        {
            MkConstraintWitness <- return $ orderMonoid @database tsel;
            readable $ DatabaseSelect (SingleTable tsel) (whereAlways @database tsel) mempty (selectRow @database tsel);
        };
    };

    data DatabaseEdit database tablesel where
    {
        DatabaseInsert :: tablesel row -> InsertClause database tablesel row -> DatabaseEdit database tablesel;
        DatabaseDelete :: tablesel row -> WhereClause database tablesel row -> DatabaseEdit database tablesel;
        DatabaseUpdate :: tablesel row -> WhereClause database tablesel row -> UpdateClause database tablesel row -> DatabaseEdit database tablesel;
    };

    instance Floating (DatabaseEdit database tablesel) (DatabaseEdit database tablesel);

    instance Database database tablesel => Edit (DatabaseEdit database tablesel) where
    {
        type EditReader (DatabaseEdit database tablesel) = DatabaseRead database tablesel;
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
