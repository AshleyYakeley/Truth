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

    data SelSchema (sel :: k -> *) (itemSchema :: k -> *) = MkSelSchema
    {
        selAllItems :: [AnyWitness sel],
        selItem :: AllF sel itemSchema
    };
    selItemLookup :: SelSchema sel itemSchema -> sel t -> itemSchema t;
    selItemLookup schema st = getAllF (selItem schema) st;

    selAllSchema :: SelSchema sel itemSchema -> [AnyWitness itemSchema];
    selAllSchema schema = fmap (\(MkAnyWitness st) -> MkAnyWitness $ selItemLookup schema st) $ selAllItems schema;

    mapSelSchema :: (forall t. i1 t -> i2 t) -> SelSchema sel i1 -> SelSchema sel i2;
    mapSelSchema ff (MkSelSchema ai (MkAllF i)) = MkSelSchema ai $ MkAllF $ \s -> ff $ i s;

    eitherSelSchema :: SelSchema sel1 itemSchema -> SelSchema sel2 itemSchema -> SelSchema (EitherWitness sel1 sel2) itemSchema;
    eitherSelSchema (MkSelSchema a1 i1) (MkSelSchema a2 i2) = MkSelSchema ((fmap (mapAnyWitness LeftWitness) a1) ++ (fmap (mapAnyWitness RightWitness) a2)) (eitherAllF i1 i2);

    instance Show (SelSchema colsel ColumnSchema) where
    {
        show schema = "(" ++ intercalate "," (fmap (\(MkAnyWitness isch) -> show isch) $ selAllSchema schema) ++ ")";
    };

    data IndexSchema colsel = MkIndexSchema
    {
        indexName :: String,
        indexColumns :: [AnyWitness colsel]
    };

    data TableSchema colsel = MkTableSchema
    {
        tableName :: String,
        tableColumns :: SelSchema colsel ColumnSchema,
        tableIndexes :: [IndexSchema colsel]
    };

    instance Show (TableSchema colsel) where
    {
        show MkTableSchema{..} = let
        {
            showIndex MkIndexSchema{..} = "CREATE INDEX IF NOT EXISTS " ++ indexName ++ " (" ++ intercalate "," (fmap (\(MkAnyWitness col) -> show $ selItemLookup tableColumns col) indexColumns) ++ ");\n"
        } in "CREATE TABLE IF NOT EXISTS " ++ tableName ++ " " ++ show tableColumns ++ ";\n" ++ (mconcat $ fmap showIndex tableIndexes);
    };

    data DatabaseSchema tablesel = MkDatabaseSchema
    {
        databaseTables :: SelSchema tablesel TableSchema
    };

    instance Show (SelSchema tablesel TableSchema) where
    {
        show MkSelSchema{..} = mconcat $ fmap (\(MkAnyWitness table) -> show $ getAllF selItem table) $ selAllItems;
    };

    instance Show (DatabaseSchema databaseTablesel) where
    {
        show MkDatabaseSchema{..} = show databaseTables;
    };

    createIfNotExists :: DatabaseSchema databaseTablesel -> Query;
    createIfNotExists schema = fromString $ show schema;
}
