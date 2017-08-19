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

    data SelSchema (itemSchema :: k -> *) (sel :: k -> *) = MkSelSchema
    {
        selAllItems :: [AnyWitness sel],
        selItem :: AllF sel itemSchema
    };

    mapSelSchema :: (forall t. i1 t -> i2 t) -> SelSchema i1 sel -> SelSchema i2 sel;
    mapSelSchema ff (MkSelSchema ai (MkAllF i)) = MkSelSchema ai $ MkAllF $ \s -> ff $ i s;

    eitherSelSchema :: SelSchema itemSchema sel1 -> SelSchema itemSchema sel2 -> SelSchema itemSchema (EitherWitness sel1 sel2);
    eitherSelSchema (MkSelSchema a1 i1) (MkSelSchema a2 i2) = MkSelSchema ((fmap (mapAnyWitness LeftWitness) a1) ++ (fmap (mapAnyWitness RightWitness) a2)) (eitherAllF i1 i2);

    instance Show (SelSchema ColumnSchema colsel) where
    {
        show MkSelSchema{..} = "(" ++ intercalate "," (fmap (\(MkAnyWitness colsel) -> show $ getAllF selItem colsel) $ selAllItems) ++ ")";
    };

    data TableSchema colsel = MkTableSchema
    {
        tableName :: String,
        tableColumns :: SelSchema ColumnSchema colsel
    };

    instance Show (TableSchema colsel) where
    {
        show MkTableSchema{..} = "CREATE TABLE IF NOT EXISTS " ++ tableName ++ " " ++ show tableColumns ++ ";";
    };

    data DatabaseSchema tablesel = MkDatabaseSchema
    {
        databaseTables :: SelSchema TableSchema tablesel
    };

    instance Show (SelSchema TableSchema tablesel) where
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
