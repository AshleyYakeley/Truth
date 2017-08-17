module Truth.Core.Types.TupleDatabase where
{
    import Truth.Core.Import;
    import Truth.Core.Edit;
    import Truth.Core.Types.Tuple;
    import Truth.Core.Types.EitherTuple;
    import Truth.Core.Types.Database;


    data TupleTableSel tablesel row where
    {
        MkTupleTableSel :: tablesel colsel -> TupleTableSel tablesel (Tuple colsel);
    };

    newtype AllTuple tablesel f = MkAllTuple (forall colsel. tablesel colsel -> f (Tuple colsel));

    class TestEquality tablesel => FiniteTupleSel (tablesel :: (* -> *) -> *) where
    {
        tupleTableAssemble :: Applicative m => (forall colsel. tablesel colsel -> m (f (Tuple colsel))) -> m (AllTuple tablesel f);
    };

    class TupleDatabase (database :: *) where
    {
        type TupleExpr database :: (* -> *) -> * -> *;
        evalTupleExpr :: TupleExpr database colsel t -> Tuple colsel -> t;
        constBoolExpr :: Bool -> TupleExpr database colsel Bool;
        columnExpr :: colsel edit -> TupleExpr database colsel (EditSubject edit);
    };

    data TupleWhereClause expr row where
    {
        MkTupleWhereClause :: expr colsel Bool -> TupleWhereClause expr (Tuple colsel);
    };

    data TupleUpdateClause expr row where
    {
        MkTupleUpdateClause :: TestEquality colsel => colsel edit -> expr colsel (EditSubject edit) -> TupleUpdateClause expr (Tuple colsel);
    };

    data TupleJoinClause rowa rowb rowc where
    {
        OuterTupleJoinClause :: TupleJoinClause (Tuple colsel1) (Tuple colsel2) (Tuple (EitherSel colsel1 colsel2));
    };

    instance TestEquality tablesel => TestEquality (TupleTableSel tablesel) where
    {
        testEquality (MkTupleTableSel selTable1) (MkTupleTableSel selTable2) = do
        {
            Refl <- testEquality selTable1 selTable2;
            return Refl;
        };
    };

    data TupleSelectClause expr row t where
    {
        MkTupleSelectClause :: (forall edit. colsel' edit -> expr colsel (EditSubject edit)) -> TupleSelectClause expr (Tuple colsel) (Tuple colsel');
    };

    data TupleOrderItem colsel where
    {
        MkTupleOrderItem :: Ord (EditSubject edit) => colsel edit -> Bool -> TupleOrderItem colsel;
    };

    data MkTupleOrderClause row where
    {
        MkMkTupleOrderClause :: [TupleOrderItem colsel] -> MkTupleOrderClause (Tuple colsel);
    };

    instance Semigroup (MkTupleOrderClause (Tuple colsel)) where
    {
        (MkMkTupleOrderClause item1) <> (MkMkTupleOrderClause item2) = MkMkTupleOrderClause $ item1 <> item2;
    };
    instance Monoid (MkTupleOrderClause (Tuple colsel)) where
    {
        mempty = MkMkTupleOrderClause mempty;
        mappend = (<>);
    };

    instance (TupleDatabase database, FiniteTupleSel tablesel) => Database database (TupleTableSel tablesel) where
    {
        tableAssemble getrow = fmap (\(MkAllTuple f) -> MkAll $ \(MkTupleTableSel tsel) -> f tsel) $ tupleTableAssemble $ \tsel -> getrow $ MkTupleTableSel tsel;

        type WhereClause database (TupleTableSel tablesel) = TupleWhereClause (TupleExpr database);
        whereClause (MkTupleWhereClause expr) = evalTupleExpr @database expr;
        whereAlways (MkTupleTableSel _) = MkTupleWhereClause $ constBoolExpr @database True;

        type InsertClause database (TupleTableSel tablesel) = [];
        insertClause = id;
        insertIntoTable _ = id;

        type UpdateClause database (TupleTableSel tablesel) = TupleUpdateClause (TupleExpr database);
        updateClause (MkTupleUpdateClause tsel expr) tuple@(MkTuple tf) = MkTuple $ \col -> case testEquality col tsel of
        {
            Just Refl -> evalTupleExpr @database expr tuple;
            Nothing -> tf col;
        };

        type OrderClause database (TupleTableSel tablesel) = MkTupleOrderClause;
        orderClause (MkMkTupleOrderClause clauses) (MkTuple tup1) (MkTuple tup2) = let
        {
            oc (MkTupleOrderItem colsel False) = compare (tup1 colsel) (tup2 colsel);
            oc (MkTupleOrderItem colsel True) = compare (Down $ tup1 colsel) (Down $ tup2 colsel);
        } in mconcat $ fmap oc clauses;
        orderMonoid (MkTupleTableSel _) = MkConstraintWitness;

        type SelectClause database (TupleTableSel tablesel) = TupleSelectClause (TupleExpr database);
        selectClause (MkTupleSelectClause selexpr) tuple = MkTuple $ \col -> evalTupleExpr @database (selexpr col) tuple;
        selectRow (MkTupleTableSel _) = MkTupleSelectClause $ columnExpr @database;

        type JoinClause database (TupleTableSel tablesel) = TupleJoinClause;
        joinClause OuterTupleJoinClause = eitherTuple;
    };
}
