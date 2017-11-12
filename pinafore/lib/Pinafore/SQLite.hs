{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE EmptyCase #-}

module Pinafore.SQLite
    ( sqlitePinaforeObject
    ) where

import Data.UUID
import Pinafore.Edit
import Shapes
import Truth.Core
import Truth.World.SQLite
import Truth.World.SQLite.Schema

instance FieldType UUID where
    fieldTypeName = "BLOB"

instance FromField UUID where
    fromField f = do
        bs <- fromField f
        maybeToOk $ fromByteString bs

instance ToField UUID where
    toField = toField . toByteString

instance FieldType Point where
    fieldTypeName = "BLOB"

instance FromField Point where
    fromField f = fmap MkPoint $ fromField f

instance ToField Point where
    toField (MkPoint uuid) = toField uuid

instance FieldType Predicate where
    fieldTypeName = "BLOB"

instance FromField Predicate where
    fromField f = fmap MkPredicate $ fromField f

instance ToField Predicate where
    toField (MkPredicate uuid) = toField uuid

data TripleTable t where
    TriplePredicate :: TripleTable Predicate
    TripleSubject :: TripleTable Point
    TripleValue :: TripleTable Point

instance FiniteWitness TripleTable where
    assembleWitnessF getw =
        (\p s v ->
             MkAllF $ \case
                 TriplePredicate -> p
                 TripleSubject -> s
                 TripleValue -> v) <$>
        getw TriplePredicate <*>
        getw TripleSubject <*>
        getw TripleValue

instance WitnessConstraint FromField TripleTable where
    witnessConstraint TriplePredicate = Dict
    witnessConstraint TripleSubject = Dict
    witnessConstraint TripleValue = Dict

instance WitnessConstraint ToField TripleTable where
    witnessConstraint TriplePredicate = Dict
    witnessConstraint TripleSubject = Dict
    witnessConstraint TripleValue = Dict

data LiteralTable t where
    LiteralKey :: LiteralTable Point
    LiteralValue :: LiteralTable Text

instance FiniteWitness LiteralTable where
    assembleWitnessF getw =
        (\k v ->
             MkAllF $ \case
                 LiteralKey -> k
                 LiteralValue -> v) <$>
        getw LiteralKey <*>
        getw LiteralValue

instance WitnessConstraint FromField LiteralTable where
    witnessConstraint LiteralKey = Dict
    witnessConstraint LiteralValue = Dict

instance WitnessConstraint ToField LiteralTable where
    witnessConstraint LiteralKey = Dict
    witnessConstraint LiteralValue = Dict

data PinaforeSchema colsel where
    PinaforeTriple :: PinaforeSchema TripleTable
    PinaforeLiteral :: PinaforeSchema LiteralTable

instance TestEquality PinaforeSchema where
    testEquality PinaforeTriple PinaforeTriple = Just Refl
    testEquality PinaforeLiteral PinaforeLiteral = Just Refl
    testEquality _ _ = Nothing

instance FiniteWitness PinaforeSchema where
    assembleWitnessF getTable =
        (\ft fl ->
             MkAllF $ \case
                 PinaforeTriple -> ft
                 PinaforeLiteral -> fl) <$>
        getTable PinaforeTriple <*>
        getTable PinaforeLiteral

instance WitnessConstraint IsSQLiteTable PinaforeSchema where
    witnessConstraint PinaforeTriple = Dict
    witnessConstraint PinaforeLiteral = Dict

soupSchema :: DatabaseSchema PinaforeSchema
soupSchema = let
    databaseTables = let
        subWitnessDomain = [MkAnyWitness PinaforeTriple, MkAnyWitness PinaforeLiteral]
        subWitnessMap :: PinaforeSchema t -> TableSchema t
        subWitnessMap PinaforeTriple = let
            tableName = "triple"
            tableColumns = let
                domain = [MkAnyWitness TripleSubject, MkAnyWitness TriplePredicate, MkAnyWitness TripleValue]
                witmap :: TripleTable t -> ColumnSchema t
                witmap TriplePredicate = MkColumnSchema "predicate" ColumnTypeNotNull True
                witmap TripleSubject = MkColumnSchema "subject" ColumnTypeNotNull True
                witmap TripleValue = MkColumnSchema "value" ColumnTypeNotNull False
                in MkSubmapWitness domain witmap
            tableIndexes = [MkIndexSchema "predval" [MkAnyWitness TriplePredicate, MkAnyWitness TripleValue]]
            in MkTableSchema {..}
        subWitnessMap PinaforeLiteral = let
            tableName = "literal"
            tableColumns = let
                domain = [MkAnyWitness LiteralKey, MkAnyWitness LiteralValue]
                witmap :: LiteralTable t -> ColumnSchema t
                witmap LiteralKey = MkColumnSchema "key" ColumnTypeNotNull True
                witmap LiteralValue = MkColumnSchema "value" ColumnTypeNotNull False
                in MkSubmapWitness domain witmap
            tableIndexes = [MkIndexSchema "litval" [MkAnyWitness LiteralValue]]
            in MkTableSchema {..}
        in MkSubmapWitness {..}
    in MkDatabaseSchema {..}

soupDatabaseLens :: GeneralLens (SQLiteEdit PinaforeSchema) PinaforeEdit
soupDatabaseLens = let
    editAccess :: IOStateAccess ()
    editAccess = unitStateAccess
    editGet :: () -> ReadFunction (SQLiteRead PinaforeSchema) PinaforeRead
    editGet () (PinaforeReadGetValue p s) = do
        row <-
            readable $
            DatabaseSelect
                (SingleTable $ MkTupleTableSel PinaforeTriple)
                (MkTupleWhereClause $
                 (ColumnExpr TriplePredicate === ConstExpr p) /\ (ColumnExpr TripleSubject === ConstExpr s))
                mempty
                (MkTupleSelectClause $ \Refl -> ColumnExpr TripleValue)
        return $ fmap getSingleAll $ listToMaybe row
    editGet () (PinaforeReadLookupValue p v) = do
        row <-
            readable $
            DatabaseSelect
                (SingleTable $ MkTupleTableSel PinaforeTriple)
                (MkTupleWhereClause $
                 (ColumnExpr TriplePredicate === ConstExpr p) /\ (ColumnExpr TripleValue === ConstExpr v))
                mempty
                (MkTupleSelectClause $ \Refl -> ColumnExpr TripleSubject)
        return $ MkFiniteSet $ fmap getSingleAll row
    editGet () (PinaforeReadGetPrimitive v) = do
        (row :: [All ((:~:) Text)]) <-
            readable $
            DatabaseSelect
                (SingleTable $ MkTupleTableSel PinaforeLiteral)
                (MkTupleWhereClause $ ColumnExpr LiteralKey === ConstExpr v)
                mempty
                (MkTupleSelectClause $ \Refl -> ColumnExpr LiteralValue)
        return $ do
            sa <- listToMaybe row
            return $ getSingleAll sa
    editGet () (PinaforeReadLookupPrimitive l) = do
        row <-
            readable $
            DatabaseSelect
                (SingleTable $ MkTupleTableSel PinaforeLiteral)
                (MkTupleWhereClause $ ColumnExpr LiteralValue === ConstExpr l)
                mempty
                (MkTupleSelectClause $ \Refl -> ColumnExpr LiteralKey)
        return $ MkFiniteSet $ fmap getSingleAll row
    editUpdate _ _ = return $ error "soupDatabaseLens.editUpdate"
    editLensPutEdit ::
           () -> PinaforeEdit -> Readable (SQLiteRead PinaforeSchema) (Maybe ((), [SQLiteEdit PinaforeSchema]))
    editLensPutEdit () (PinaforeEditSetValue p s (Just v)) =
        pure $
        pure $
        pure $
        pure $
        DatabaseInsert (MkTupleTableSel PinaforeTriple) $
        MkTupleInsertClause $
        pure $
        MkAll $ \case
            TriplePredicate -> p
            TripleSubject -> s
            TripleValue -> v
    editLensPutEdit () (PinaforeEditSetValue p s Nothing) =
        pure $
        pure $
        pure $
        pure $
        DatabaseDelete (MkTupleTableSel PinaforeTriple) $
        MkTupleWhereClause $ ColumnExpr TriplePredicate === ConstExpr p /\ ColumnExpr TripleSubject === ConstExpr s
    editLensPutEdit () (PinaforeEditSetPrimitive v (Just l)) =
        pure $
        pure $
        pure $
        pure $
        DatabaseInsert (MkTupleTableSel PinaforeLiteral) $
        MkTupleInsertClause $
        pure $
        MkAll $ \case
            LiteralKey -> v
            LiteralValue -> l
    editLensPutEdit () (PinaforeEditSetPrimitive v Nothing) =
        pure $
        pure $
        pure $
        pure $
        DatabaseDelete (MkTupleTableSel PinaforeLiteral) $ MkTupleWhereClause $ ColumnExpr LiteralKey === ConstExpr v
    editLensFunction = MkEditFunction {..}
    in MkCloseState $ MkEditLens {..}

sqlitePinaforeObject :: FilePath -> Object PinaforeEdit
sqlitePinaforeObject path = mapObject soupDatabaseLens $ sqliteObject path soupSchema
