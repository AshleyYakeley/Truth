{-# OPTIONS -fno-warn-orphans #-}

module Changes.World.SQLite.Schema where

import Data.Time
import Database.SQLite.Simple hiding (columnName)
import Shapes
import Shapes.Numeric

class FieldType (t :: Type) where
    fieldTypeName :: String

instance FieldType SQLData where
    fieldTypeName = ""

instance FieldType Int64 where
    fieldTypeName = "INT"

instance FieldType Int where
    fieldTypeName = "INT"

instance FieldType Text where
    fieldTypeName = "TEXT"

instance FieldType LazyByteString where
    fieldTypeName = "BLOB"

instance FieldType StrictByteString where
    fieldTypeName = "BLOB"

instance FieldType Double where
    fieldTypeName = "DOUBLE"

instance FieldType Bool where
    fieldTypeName = "BOOLEAN"

instance FieldType Day where
    fieldTypeName = "DATE"

instance FieldType UTCTime where
    fieldTypeName = "DATETIME"

data ColumnTypeSchema t where
    ColumnTypeNotNull :: FieldType t => ColumnTypeSchema t
    ColumnTypeMaybeNull :: FieldType t => ColumnTypeSchema (Maybe t)

instance Show (ColumnTypeSchema t) where
    show ColumnTypeNotNull = fieldTypeName @t ++ " NOT NULL"
    show ColumnTypeMaybeNull = let
        n ::
            forall a.
            Maybe a ~ t =>
            String
        n = fieldTypeName @a
        in n

data ColumnSchema t = MkColumnSchema
    { columnName :: String
    , columnType :: ColumnTypeSchema t
    , columnPrimaryKey :: Bool
    }

instance Show (ColumnSchema t) where
    show MkColumnSchema{..} = columnName ++ " " ++ show columnType

instance Show (FiniteAllFor ColumnSchema colsel) where
    show schema = let
        columns = finiteCodomain schema
        in "("
            ++ intercalate "," (fmap (\(MkSome isch) -> show isch) $ columns)
            ++ ",PRIMARY KEY ("
            ++ intercalate
                ","
                ( mapMaybe
                    ( \(MkSome MkColumnSchema{..}) ->
                        if columnPrimaryKey
                            then Just columnName
                            else Nothing
                    )
                    columns
                )
            ++ "))"

class ToSchema t where
    toSchema :: t -> [Query]

data IndexSchema (colsel :: Type -> Type) = MkIndexSchema
    { indexName :: String
    , indexColumns :: [Some colsel]
    }

data TableSchema (colsel :: Type -> Type) = MkTableSchema
    { tableName :: String
    , tableColumns :: FiniteAllFor ColumnSchema colsel
    , tableIndexes :: [IndexSchema colsel]
    }

instance ToSchema (TableSchema colsel) where
    toSchema MkTableSchema{..} = let
        createTable = fromString $ "CREATE TABLE IF NOT EXISTS " ++ tableName ++ " " ++ show tableColumns
        showIndex MkIndexSchema{..} =
            fromString
                $ "CREATE INDEX IF NOT EXISTS "
                ++ indexName
                ++ " ON "
                ++ tableName
                ++ " ("
                ++ intercalate "," (fmap (\(MkSome col) -> columnName $ finiteGetAllFor tableColumns col) indexColumns)
                ++ ")"
        in createTable : (fmap showIndex tableIndexes)

data DatabaseSchema tablesel = MkDatabaseSchema
    { databaseTables :: FiniteAllFor TableSchema tablesel
    }

instance ToSchema (FiniteAllFor TableSchema tablesel) where
    toSchema MkFiniteAllFor{..} = concatmap (\(MkSome table) -> toSchema $ finiteGetAllFor table) $ finiteDomain

instance ToSchema (DatabaseSchema databaseTablesel) where
    toSchema MkDatabaseSchema{..} = toSchema databaseTables
