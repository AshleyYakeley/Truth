module Truth.World.SQLite where
{
    import Truth.Core.Import;
    import Truth.Core;


    class FiniteTupleSel tablesel => NamedTupleSel (tablesel :: (* -> *) -> *) where
    {
        tableName :: tablesel colsel -> String;
        columnName :: tablesel colsel -> colsel edit -> String;
    };

    data SQLPrimitiveType t where
    {
        SQLBool :: SQLPrimitiveType Bool;
        SQLInt :: SQLPrimitiveType Int64;
        SQLFloat :: SQLPrimitiveType Double;
        SQLString :: SQLPrimitiveType String;
    };

    data SQLExpr colsel t where
    {
        ConstSQLExpr :: SQLPrimitiveType t -> t -> SQLExpr colsel t;
        ColumnSQLExpr :: colsel edit -> SQLExpr colsel (EditSubject edit);
        EqualsSQLExpr :: Eq t => SQLExpr colsel t -> SQLExpr colsel t -> SQLExpr colsel Bool;
    };

    data SQLiteDatabase;

    instance TupleDatabase SQLiteDatabase where
    {
        type TupleExpr SQLiteDatabase = SQLExpr;

        evalTupleExpr (ConstSQLExpr _ v) _ = v;
        evalTupleExpr (ColumnSQLExpr sel) (MkTuple tuple) = tuple sel;
        evalTupleExpr (EqualsSQLExpr e1 e2) tuple = (evalTupleExpr @SQLiteDatabase e1 tuple) == (evalTupleExpr @SQLiteDatabase e2 tuple);

        constBoolExpr = ConstSQLExpr SQLBool;
        columnExpr = ColumnSQLExpr;
    };
}
