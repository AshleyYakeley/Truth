{-# OPTIONS -fno-warn-orphans #-}
module Truth.World.SQLite.Schema where
{
    import Truth.Core.Import;
    import Data.Time;
    import Database.SQLite.Simple hiding (columnName);


    data BasicTypeSchema t where
    {
        TypeINT :: BasicTypeSchema Int64;
        TypeTEXT :: BasicTypeSchema Text;
        TypeBLOB :: BasicTypeSchema ByteString;
        TypeDOUBLE :: BasicTypeSchema Double;
        TypeBOOLEAN :: BasicTypeSchema Bool;
        TypeDATE :: BasicTypeSchema Day;
        TypeDATETIME :: BasicTypeSchema UTCTime;
    };

    instance Show (BasicTypeSchema t) where
    {
        show TypeINT = "INT";
        show TypeTEXT = "TEXT";
        show TypeBLOB = "BLOB";
        show TypeDOUBLE = "DOUBLE";
        show TypeBOOLEAN = "BOOLEAN";
        show TypeDATE = "DATE";
        show TypeDATETIME = "DATETIME";
    };

    data ColumnTypeSchema t where
    {
        ColumnTypeNotNull :: BasicTypeSchema t -> ColumnTypeSchema t;
        ColumnTypeMaybeNull :: BasicTypeSchema t -> ColumnTypeSchema (Maybe t);
    };

    instance Show (ColumnTypeSchema t) where
    {
        show (ColumnTypeNotNull bts) = show bts ++ " NOT NULL";
        show (ColumnTypeMaybeNull bts) = show bts;
    };

    data ColumnSchema t = MkColumnSchema
    {
        columnName :: String,
        columnType :: ColumnTypeSchema t,
        columnPrimaryKey :: Bool
    };

    instance Show (ColumnSchema t) where
    {
        show MkColumnSchema{..} = columnName ++ " " ++ show columnType ++ if columnPrimaryKey then " PRIMARY KEY" else "";
    };

    instance Show (SubmapWitness colsel ColumnSchema) where
    {
        show schema = "(" ++ intercalate "," (fmap (\(MkAnyWitness isch) -> show isch) $ subWitnessCodomain schema) ++ ")";
    };

    data IndexSchema colsel = MkIndexSchema
    {
        indexName :: String,
        indexColumns :: [AnyWitness colsel]
    };

    data TableSchema colsel = MkTableSchema
    {
        tableName :: String,
        tableColumns :: SubmapWitness colsel ColumnSchema,
        tableIndexes :: [IndexSchema colsel]
    };

    instance Show (TableSchema colsel) where
    {
        show MkTableSchema{..} = let
        {
            showIndex MkIndexSchema{..} = "CREATE INDEX IF NOT EXISTS " ++ indexName ++ " (" ++ intercalate "," (fmap (\(MkAnyWitness col) -> show $ subWitnessMap tableColumns col) indexColumns) ++ ");\n"
        } in "CREATE TABLE IF NOT EXISTS " ++ tableName ++ " " ++ show tableColumns ++ ";\n" ++ (mconcat $ fmap showIndex tableIndexes);
    };

    data DatabaseSchema tablesel = MkDatabaseSchema
    {
        databaseTables :: SubmapWitness tablesel TableSchema
    };

    instance Show (SubmapWitness tablesel TableSchema) where
    {
        show MkSubmapWitness{..} = mconcat $ fmap (\(MkAnyWitness table) -> show $ subWitnessMap table) $ subWitnessDomain;
    };

    instance Show (DatabaseSchema databaseTablesel) where
    {
        show MkDatabaseSchema{..} = show databaseTables;
    };

    createIfNotExists :: DatabaseSchema databaseTablesel -> Query;
    createIfNotExists schema = fromString $ show schema;
}
