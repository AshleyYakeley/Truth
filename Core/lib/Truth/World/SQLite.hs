{-# OPTIONS -fno-warn-orphans #-}
module Truth.World.SQLite where
{
    import Truth.Core.Import;
    import Database.SQLite.Simple hiding (columnName);
    import Database.SQLite.Simple.FromField;
    import Database.SQLite.Simple.ToField;
    import Truth.Core;
    import qualified Truth.World.SQLite.Schema as SQLite;


    data SQLiteDatabase;
    type SQLiteRead tablesel = TupleDatabaseRead SQLiteDatabase tablesel;
    type SQLiteEdit tablesel = TupleDatabaseEdit SQLiteDatabase tablesel;

    type family RowColSel (row :: *) :: * -> *;
    type instance RowColSel (All colsel) = colsel;

    class HasSchema (t :: *) where
    {
        type Schema t :: *;
        schemaString :: Schema t -> t -> String;
    };

    data Expr colsel t where
    {
        ConstExpr :: ToField t => t -> Expr colsel t;
        ColumnExpr :: colsel t -> Expr colsel t;
        EqualsExpr :: Eq t => Expr colsel t -> Expr colsel t -> Expr colsel Bool;
    };

    evalExpr :: Expr colsel t -> (forall a. colsel a -> a) -> t;
    evalExpr (ConstExpr v) _ = v;
    evalExpr (ColumnExpr sel) tuple = tuple sel;
    evalExpr (EqualsExpr e1 e2) tuple = (evalExpr e1 tuple) == (evalExpr e2 tuple);

    data ColumnRefSchema t = MkColumnRefSchema
    {
        columnRefName :: String,
        columnRefType :: SQLite.ColumnTypeSchema t
    };

    instance HasSchema (Expr colsel t) where
    {
        type Schema (Expr colsel t) = SQLite.SelSchema ColumnRefSchema colsel;
        schemaString _ (ConstExpr t) = show $ toField t;
        schemaString csch (ColumnExpr col) = columnRefName $ getAllF (SQLite.selItem csch) col;
        schemaString csch (EqualsExpr e1 e2) = schemaString csch e1 ++ "=" ++ schemaString csch e2;
    };

    class (FiniteWitness colsel,WitnessConstraint FromField colsel) => IsSQLiteTable colsel;
    instance (FiniteWitness colsel,WitnessConstraint FromField colsel) => IsSQLiteTable colsel;

    instance TupleDatabase SQLiteDatabase where
    {
        type TupleDatabaseRowWitness SQLiteDatabase = IsSQLiteTable;

        type TupleExpr SQLiteDatabase colsel = Expr colsel;
        evalTupleExpr expr (MkAll tuple) = evalExpr expr tuple;
        constBoolExpr = ConstExpr;
        columnExpr = ColumnExpr;
    };

    instance (FiniteWitness colsel,WitnessConstraint FromField colsel) => FromRow (All colsel) where
    {
        fromRow = assembleWitness $ \wt -> case witnessConstraint @Type @FromField wt of
        {
            MkConstraintWitness -> fmap fromOnly fromRow;
        };
    };

    instance HasSchema (TupleWhereClause SQLiteDatabase row) where
    {
        type Schema (TupleWhereClause SQLiteDatabase row) = SQLite.SelSchema ColumnRefSchema (RowColSel row);
        schemaString csch (MkTupleWhereClause expr) = schemaString csch expr;
    };

    instance HasSchema (TupleSelectClause SQLiteDatabase row row') where
    {
        type Schema (TupleSelectClause SQLiteDatabase row row') = SQLite.SelSchema ColumnRefSchema (RowColSel row);
        schemaString csch (MkTupleSelectClause mapSel) = intercalate "," $ fmap (\(MkAnyWitness cs') -> schemaString csch $ mapSel cs') $ allWitnesses @(RowColSel row');
    };

    instance HasSchema (TupleOrderItem colsel) where
    {
        type Schema (TupleOrderItem colsel) = SQLite.SelSchema ColumnRefSchema colsel;
        schemaString csch (MkTupleOrderItem col dir) = (columnRefName $ getAllF (SQLite.selItem csch) col) ++ " " ++ show dir;
    };

    columnRef :: String -> SQLite.ColumnSchema t -> ColumnRefSchema t;
    columnRef tableRefName SQLite.MkColumnSchema{..} = let
    {
        columnRefName = tableRefName ++ "." ++ columnName;
        columnRefType = columnType;
    } in MkColumnRefSchema{..};

    joinTableSchema :: forall tablesel row. SQLite.SelSchema SQLite.TableSchema tablesel -> Join SQLiteDatabase (TupleTableSel tablesel) row -> State Int ([String],SQLite.SelSchema ColumnRefSchema (RowColSel row));
    joinTableSchema schema (SingleTable (MkTupleTableSel tsel)) = do
    {
        i <- get;
        put $ i + 1;
        let
        {
            tableRefName = "t" ++ show i;
            SQLite.MkTableSchema{..} = getAllF (SQLite.selItem schema) tsel;
            tabRefText = tableName ++ " AS " ++ tableRefName;
            colRefSchema = SQLite.mapSelSchema (columnRef tableRefName) tableColumns;
        };
        return ([tabRefText],colRefSchema);
    };
    joinTableSchema schema (JoinTables OuterTupleJoinClause j1 j2) = do
    {
        (t1,s1) <- joinTableSchema schema j1;
        (t2,s2) <- joinTableSchema schema j2;
        return $ (t1 ++ t2,SQLite.eitherSelSchema s1 s2);
    };

    sqliteObject :: forall tablesel. FiniteTupleDatabaseSel tablesel => FilePath -> SQLite.DatabaseSchema tablesel -> Object (SQLiteEdit tablesel);
    sqliteObject path SQLite.MkDatabaseSchema{..} = let
    {
        muted :: Connection -> MutableEdit IO (DatabaseEdit SQLiteDatabase (TupleTableSel tablesel));
        muted conn = let
        {
            sqliteReadQuery :: SQLiteRead tablesel [All colsel] -> Query;
            sqliteReadQuery (DatabaseSelect jc wc oc sc) = case evalState (joinTableSchema databaseTables jc) 1 of
            {
                (tabRefs,rowSchema) -> let
                {
                    fromPart = case tabRefs of
                    {
                        [] -> "";
                        _ -> " FROM " ++ intercalate "," tabRefs;
                    };

                    wherePart = case wc of
                    {
                        MkTupleWhereClause (ConstExpr True) -> "";
                        _ -> " WHERE " ++ schemaString rowSchema wc;
                    };

                    orderPart = case oc of
                    {
                        MkTupleOrderClause [] -> "";
                        MkTupleOrderClause ocs -> " ORDER BY " ++ (intercalate "," $ fmap (schemaString rowSchema) ocs);
                    };
                } in fromString $ "SELECT " ++ schemaString rowSchema sc ++ fromPart ++ wherePart ++ orderPart;
            };

            sqliteEditQuery :: SQLiteEdit tablesel -> Query;
            sqliteEditQuery = undefined;

            mutableRead :: MutableRead IO (SQLiteRead tablesel);
            mutableRead r@(DatabaseSelect _ _ _ (MkTupleSelectClause _)) = query_ conn $ sqliteReadQuery r;

            mutableEdit :: [SQLiteEdit tablesel] -> IO (Maybe (IO ()));
            mutableEdit = singleAlwaysMutableEdit $ \edit -> execute_ conn $ sqliteEditQuery edit;
        } in MkMutableEdit{..};
    } in MkObject $ \call -> withConnection path $ \conn -> call $ muted conn;
}
