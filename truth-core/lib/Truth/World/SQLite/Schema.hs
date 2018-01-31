{-# OPTIONS -fno-warn-orphans #-}

module Truth.World.SQLite.Schema where

import Data.Time
import Database.SQLite.Simple hiding (columnName)
import Truth.Core.Import

class FieldType t where
    fieldTypeName :: String

instance FieldType SQLData where
    fieldTypeName = ""

instance FieldType Int64 where
    fieldTypeName = "INT"

instance FieldType Text where
    fieldTypeName = "TEXT"

instance FieldType ByteString where
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
        n :: forall a. (Maybe a ~ t)
          => String
        n = fieldTypeName @a
        in n

data ColumnSchema t = MkColumnSchema
    { columnName :: String
    , columnType :: ColumnTypeSchema t
    , columnPrimaryKey :: Bool
    }

instance Show (ColumnSchema t) where
    show MkColumnSchema {..} = columnName ++ " " ++ show columnType

instance Show (SubmapWitness colsel ColumnSchema) where
    show schema = let
        columns = subWitnessCodomain schema
        in "(" ++
           intercalate "," (fmap (\(MkAnyWitness isch) -> show isch) $ columns) ++
           ",PRIMARY KEY (" ++
           intercalate
               ","
               (mapMaybe
                    (\(MkAnyWitness MkColumnSchema {..}) ->
                         if columnPrimaryKey
                             then Just columnName
                             else Nothing)
                    columns) ++
           "))"

class ToSchema t where
    toSchema :: t -> [Query]

data IndexSchema colsel = MkIndexSchema
    { indexName :: String
    , indexColumns :: [AnyWitness colsel]
    }

data TableSchema colsel = MkTableSchema
    { tableName :: String
    , tableColumns :: SubmapWitness colsel ColumnSchema
    , tableIndexes :: [IndexSchema colsel]
    }

instance ToSchema (TableSchema colsel) where
    toSchema MkTableSchema {..} = let
        createTable = fromString $ "CREATE TABLE IF NOT EXISTS " ++ tableName ++ " " ++ show tableColumns
        showIndex MkIndexSchema {..} =
            fromString $
            "CREATE INDEX IF NOT EXISTS " ++
            indexName ++
            " ON " ++
            tableName ++
            " (" ++
            intercalate "," (fmap (\(MkAnyWitness col) -> columnName $ subWitnessMap tableColumns col) indexColumns) ++
            ")"
        in createTable : (fmap showIndex tableIndexes)

data DatabaseSchema tablesel = MkDatabaseSchema
    { databaseTables :: SubmapWitness tablesel TableSchema
    }

instance ToSchema (SubmapWitness tablesel TableSchema) where
    toSchema MkSubmapWitness {..} =
        mconcat $ fmap (\(MkAnyWitness table) -> toSchema $ subWitnessMap table) $ subWitnessDomain

instance ToSchema (DatabaseSchema databaseTablesel) where
    toSchema MkDatabaseSchema {..} = toSchema databaseTables