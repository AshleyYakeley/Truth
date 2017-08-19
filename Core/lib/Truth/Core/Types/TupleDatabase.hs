module Truth.Core.Types.TupleDatabase where
{
    import Truth.Core.Import;
    import Truth.Core.Types.EitherTuple;
    import Truth.Core.Types.Database;


    data TupleTableSel tablesel row where
    {
        MkTupleTableSel :: tablesel colsel -> TupleTableSel tablesel (All colsel);
    };

    newtype AllTuple tablesel f = MkAllTuple (forall colsel. tablesel colsel -> f (All colsel));

    class TestEquality tablesel => FiniteTupleDatabaseSel (tablesel :: (* -> *) -> *) where
    {
        tupleAssembleTables :: Applicative m => (forall colsel. tablesel colsel -> m (f (All colsel))) -> m (AllTuple tablesel f);
        tupleTableAssembleColumns :: Applicative m => tablesel colsel -> (forall t. colsel t -> m t) -> m (All colsel);
    };

    tupleTables :: FiniteTupleDatabaseSel tablesel => [AnyWitness tablesel];
    tupleTables = execWriter $ tupleAssembleTables $ \table -> do
    {
        tell [MkAnyWitness table];
        return $ Const ();
    };

    class TupleDatabase (database :: *) where
    {
        type TupleDatabaseRowWitness database :: (* -> *) -> Constraint;

        type TupleExpr database (colsel :: * -> *) :: * -> *;
        evalTupleExpr :: TupleExpr database colsel t -> All colsel -> t;
        constBoolExpr :: Bool -> TupleExpr database colsel Bool;
        columnExpr :: colsel t -> TupleExpr database colsel t;
    };

    type TupleDatabaseRead database tablesel = DatabaseRead database (TupleTableSel tablesel);
    type TupleDatabaseEdit database tablesel = DatabaseEdit database (TupleTableSel tablesel);

    data TupleWhereClause database row where
    {
        MkTupleWhereClause :: TupleExpr database colsel Bool -> TupleWhereClause database (All colsel);
    };

    data TupleUpdateClause database row where
    {
        MkTupleUpdateClause :: TestEquality colsel => colsel t -> TupleExpr database colsel t -> TupleUpdateClause database (All colsel);
    };

    data TupleJoinClause rowa rowb rowc where
    {
        OuterTupleJoinClause :: TupleJoinClause (All colsel1) (All colsel2) (All (EitherSel colsel1 colsel2));
    };

    instance TestEquality tablesel => TestEquality (TupleTableSel tablesel) where
    {
        testEquality (MkTupleTableSel selTable1) (MkTupleTableSel selTable2) = do
        {
            Refl <- testEquality selTable1 selTable2;
            return Refl;
        };
    };

    data TupleSelectClause database row t where
    {
        MkTupleSelectClause :: TupleDatabaseRowWitness database colsel' => (forall t. colsel' t -> TupleExpr database colsel t) -> TupleSelectClause database (All colsel) (All colsel');
    };

    data SortDir = SortAsc | SortDesc deriving (Eq);
    instance Show SortDir where
    {
        show SortAsc = "ASC";
        show SortDesc = "DESC";
    };

    data TupleOrderItem colsel where
    {
        MkTupleOrderItem :: Ord t => colsel t -> SortDir -> TupleOrderItem colsel;
    };

    data TupleOrderClause row where
    {
        MkTupleOrderClause :: [TupleOrderItem colsel] -> TupleOrderClause (All colsel);
    };

    instance Semigroup (TupleOrderClause (All colsel)) where
    {
        (MkTupleOrderClause item1) <> (MkTupleOrderClause item2) = MkTupleOrderClause $ item1 <> item2;
    };
    instance Monoid (TupleOrderClause (All colsel)) where
    {
        mempty = MkTupleOrderClause mempty;
        mappend = (<>);
    };

    instance (WitnessConstraint (TupleDatabaseRowWitness database) tablesel,TupleDatabase database, FiniteTupleDatabaseSel tablesel) => Database database (TupleTableSel tablesel) where
    {
        tableAssemble getrow = fmap (\(MkAllTuple f) -> MkAllF $ \(MkTupleTableSel tsel) -> f tsel) $ tupleAssembleTables $ \tsel -> getrow $ MkTupleTableSel tsel;

        type WhereClause database (TupleTableSel tablesel) row = TupleWhereClause database row;
        whereClause (MkTupleWhereClause expr) = evalTupleExpr @database expr;
        whereAlways (MkTupleTableSel (_ :: tablesel colsel)) = MkTupleWhereClause $ constBoolExpr @database @colsel True;

        type InsertClause database (TupleTableSel tablesel) row = [row];
        insertClause = id;
        insertIntoTable _ = id;

        type UpdateClause database (TupleTableSel tablesel) row = TupleUpdateClause database row;
        updateClause (MkTupleUpdateClause tsel expr) tuple@(MkAll tf) = MkAll $ \col -> case testEquality col tsel of
        {
            Just Refl -> evalTupleExpr @database expr tuple;
            Nothing -> tf col;
        };

        type OrderClause database (TupleTableSel tablesel) row = TupleOrderClause row;
        orderClause (MkTupleOrderClause clauses) (MkAll tup1) (MkAll tup2) = let
        {
            oc (MkTupleOrderItem colsel SortAsc) = compare (tup1 colsel) (tup2 colsel);
            oc (MkTupleOrderItem colsel SortDesc) = compare (Down $ tup1 colsel) (Down $ tup2 colsel);
        } in mconcat $ fmap oc clauses;
        orderMonoid (MkTupleTableSel _) = MkConstraintWitness;

        type SelectClause database (TupleTableSel tablesel) = TupleSelectClause database;
        selectClause (MkTupleSelectClause selexpr) tuple = MkAll $ \col -> evalTupleExpr @database (selexpr col) tuple;
        selectRow (MkTupleTableSel tsel) = case witnessConstraint @_ @(TupleDatabaseRowWitness database) tsel of
        {
            MkConstraintWitness -> MkTupleSelectClause $ columnExpr @database;
        };

        type JoinClause database (TupleTableSel tablesel) = TupleJoinClause;
        joinClause OuterTupleJoinClause = eitherAll;
    };
}
