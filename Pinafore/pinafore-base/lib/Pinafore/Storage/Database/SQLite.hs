{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Storage.Database.SQLite
    ( sqlitePinaforeTableReference
    , sqlitePinaforeTableGetEntireDatabase
    ) where

import Changes.Core
import Changes.World.SQLite
import Changes.World.SQLite.Schema
import Pinafore.Base
import Pinafore.Storage.Database
import Pinafore.Storage.Table
import Shapes

instance FieldType Anchor where
    fieldTypeName = "BLOB"

instance FromField Anchor where
    fromField f = do
        bs <- fromField f
        decode serializeStrictCodec bs

instance ToField Anchor where
    toField = toField . encodeM serializeStrictCodec

instance FieldType Entity where
    fieldTypeName = fieldTypeName @Anchor

deriving instance FromField Entity

deriving instance ToField Entity

instance FieldType Predicate where
    fieldTypeName = fieldTypeName @Anchor

deriving instance FromField Predicate

deriving instance ToField Predicate

instance FieldType Literal where
    fieldTypeName = "BLOB"

instance FromField Literal where
    fromField f = do
        bs <- fromField f
        decode serializeStrictCodec bs

instance ToField Literal where
    toField = toField . encodeM serializeStrictCodec

instance WitnessConstraint FromField TripleTable where
    witnessConstraint TriplePredicate = Dict
    witnessConstraint TripleSubject = Dict
    witnessConstraint TripleValue = Dict

instance WitnessConstraint ToField TripleTable where
    witnessConstraint TriplePredicate = Dict
    witnessConstraint TripleSubject = Dict
    witnessConstraint TripleValue = Dict

instance WitnessConstraint FromField LiteralTable where
    witnessConstraint LiteralKey = Dict
    witnessConstraint LiteralValue = Dict

instance WitnessConstraint ToField LiteralTable where
    witnessConstraint LiteralKey = Dict
    witnessConstraint LiteralValue = Dict

instance WitnessConstraint FromField RefCountTable where
    witnessConstraint RefCountKey = Dict
    witnessConstraint RefCountValue = Dict

instance WitnessConstraint ToField RefCountTable where
    witnessConstraint RefCountKey = Dict
    witnessConstraint RefCountValue = Dict

instance WitnessConstraint IsSQLiteTable PinaforeSchema where
    witnessConstraint PinaforeProperty = Dict
    witnessConstraint PinaforeModelCount = Dict
    witnessConstraint PinaforeFact = Dict
    witnessConstraint PinaforeLiteral = Dict

sqlitePinaforeSchema :: DatabaseSchema PinaforeSchema
sqlitePinaforeSchema = let
    databaseTables =
        mkFiniteAllFor @TableSchema $ \case
            PinaforeProperty -> let
                tableName = "property"
                tableColumns =
                    mkFiniteAllFor @ColumnSchema $ \case
                        TriplePredicate -> MkColumnSchema "predicate" ColumnTypeNotNull True
                        TripleSubject -> MkColumnSchema "subject" ColumnTypeNotNull True
                        TripleValue -> MkColumnSchema "value" ColumnTypeNotNull False
                tableIndexes = [MkIndexSchema "propval" [MkSome TriplePredicate, MkSome TripleValue]]
                in MkTableSchema {..}
            PinaforeModelCount -> let
                tableName = "refcount"
                tableColumns =
                    mkFiniteAllFor @ColumnSchema $ \case
                        RefCountKey -> MkColumnSchema "key" ColumnTypeNotNull True
                        RefCountValue -> MkColumnSchema "value" ColumnTypeNotNull False
                tableIndexes = []
                in MkTableSchema {..}
            PinaforeFact -> let
                tableName = "fact"
                tableColumns =
                    mkFiniteAllFor @ColumnSchema $ \case
                        TriplePredicate -> MkColumnSchema "predicate" ColumnTypeNotNull True
                        TripleSubject -> MkColumnSchema "subject" ColumnTypeNotNull True
                        TripleValue -> MkColumnSchema "value" ColumnTypeNotNull False
                tableIndexes = [MkIndexSchema "factval" [MkSome TriplePredicate, MkSome TripleValue]]
                in MkTableSchema {..}
            PinaforeLiteral -> let
                tableName = "literal"
                tableColumns =
                    mkFiniteAllFor @ColumnSchema $ \case
                        LiteralKey -> MkColumnSchema "key" ColumnTypeNotNull True
                        LiteralValue -> MkColumnSchema "value" ColumnTypeNotNull False
                tableIndexes = [MkIndexSchema "litval" [MkSome LiteralValue]]
                in MkTableSchema {..}
    in MkDatabaseSchema {..}

class (FiniteWitness colsel, WitnessConstraint Show colsel, AllConstraint Show colsel) =>
          IsPinaforeRow (colsel :: Type -> Type)

instance (FiniteWitness colsel, WitnessConstraint Show colsel, AllConstraint Show colsel) => IsPinaforeRow colsel

instance TupleDatabase SQLiteDatabase PinaforeSchema where
    type TupleDatabaseRowWitness SQLiteDatabase PinaforeSchema = IsPinaforeRow

sqlitePinaforeLens :: ChangeLens (SQLiteUpdate PinaforeSchema) PinaforeTableUpdate
sqlitePinaforeLens = let
    clRead :: ReadFunction (SQLiteReader PinaforeSchema) PinaforeTableRead
    clRead mr (PinaforeTableReadPropertyGet p s) = do
        row <-
            mr $
            DatabaseSelect
                (SingleTable $ MkTupleTableSel PinaforeProperty)
                (MkTupleWhereClause $
                 (ColumnExpr TriplePredicate === ConstExpr p) /\ (ColumnExpr TripleSubject === ConstExpr s))
                mempty
                (MkTupleSelectClause $ \Refl -> ColumnExpr TripleValue)
        return $ fmap getSingleAllOf $ listToMaybe row
    clRead mr (PinaforeTableReadPropertyLookup p v) = do
        row <-
            mr $
            DatabaseSelect
                (SingleTable $ MkTupleTableSel PinaforeProperty)
                (MkTupleWhereClause $
                 (ColumnExpr TriplePredicate === ConstExpr p) /\ (ColumnExpr TripleValue === ConstExpr v))
                mempty
                (MkTupleSelectClause $ \Refl -> ColumnExpr TripleSubject)
        return $ MkFiniteSet $ fmap getSingleAllOf row
    clRead mr (PinaforeTableReadEntityRefCount v) = do
        (row :: [AllOf ((:~:) RefCount)]) <-
            mr $
            DatabaseSelect
                (SingleTable $ MkTupleTableSel PinaforeModelCount)
                (MkTupleWhereClause $ ColumnExpr RefCountKey === ConstExpr v)
                mempty
                (MkTupleSelectClause $ \Refl -> ColumnExpr RefCountValue)
        return $ do
            sa <- listToMaybe row
            return $ getSingleAllOf sa
    clRead mr (PinaforeTableReadFactGet p s) = do
        row <-
            mr $
            DatabaseSelect
                (SingleTable $ MkTupleTableSel PinaforeFact)
                (MkTupleWhereClause $
                 (ColumnExpr TriplePredicate === ConstExpr p) /\ (ColumnExpr TripleSubject === ConstExpr s))
                mempty
                (MkTupleSelectClause $ \Refl -> ColumnExpr TripleValue)
        return $ fmap getSingleAllOf $ listToMaybe row
    clRead mr (PinaforeTableReadLiteralGet v) = do
        (row :: [AllOf ((:~:) Literal)]) <-
            mr $
            DatabaseSelect
                (SingleTable $ MkTupleTableSel PinaforeLiteral)
                (MkTupleWhereClause $ ColumnExpr LiteralKey === ConstExpr v)
                mempty
                (MkTupleSelectClause $ \Refl -> ColumnExpr LiteralValue)
        return $ do
            sa <- listToMaybe row
            return $ getSingleAllOf sa
    clUpdate ::
           forall m. MonadIO m
        => SQLiteUpdate PinaforeSchema
        -> Readable m (EditReader (SQLiteEdit PinaforeSchema))
        -> m [PinaforeTableUpdate]
    clUpdate _ _ = return $ error "sqlitePinaforeLens.editUpdate"
    clPutEdit ::
           forall m. MonadIO m
        => PinaforeTableEdit
        -> m (Maybe [SQLiteEdit PinaforeSchema])
    clPutEdit (PinaforeTableEditPropertySet p s (Just v)) =
        return $
        Just $
        pure $
        DatabaseInsert (MkTupleTableSel PinaforeProperty) $
        MkTupleInsertClause $
        pure $
        MkAllOf $ \case
            TriplePredicate -> p
            TripleSubject -> s
            TripleValue -> v
    clPutEdit (PinaforeTableEditPropertySet p s Nothing) =
        return $
        Just $
        pure $
        DatabaseDelete (MkTupleTableSel PinaforeProperty) $
        MkTupleWhereClause $ ColumnExpr TriplePredicate === ConstExpr p /\ ColumnExpr TripleSubject === ConstExpr s
    clPutEdit (PinaforeTableEditEntityRefCount v (Just rc)) =
        return $
        Just $
        pure $
        DatabaseInsert (MkTupleTableSel PinaforeModelCount) $
        MkTupleInsertClause $
        pure $
        MkAllOf $ \case
            RefCountKey -> v
            RefCountValue -> rc
    clPutEdit (PinaforeTableEditEntityRefCount v Nothing) =
        return $
        Just $
        pure $
        DatabaseDelete (MkTupleTableSel PinaforeModelCount) $
        MkTupleWhereClause $ ColumnExpr RefCountKey === ConstExpr v
    clPutEdit (PinaforeTableEditFactSet p s (Just v)) =
        return $
        Just $
        pure $
        DatabaseInsert (MkTupleTableSel PinaforeFact) $
        MkTupleInsertClause $
        pure $
        MkAllOf $ \case
            TriplePredicate -> p
            TripleSubject -> s
            TripleValue -> v
    clPutEdit (PinaforeTableEditFactSet p s Nothing) =
        return $
        Just $
        pure $
        DatabaseDelete (MkTupleTableSel PinaforeFact) $
        MkTupleWhereClause $ ColumnExpr TriplePredicate === ConstExpr p /\ ColumnExpr TripleSubject === ConstExpr s
    clPutEdit (PinaforeTableEditLiteralSet v (Just l)) =
        return $
        Just $
        pure $
        DatabaseInsert (MkTupleTableSel PinaforeLiteral) $
        MkTupleInsertClause $
        pure $
        MkAllOf $ \case
            LiteralKey -> v
            LiteralValue -> l
    clPutEdit (PinaforeTableEditLiteralSet v Nothing) =
        return $
        Just $
        pure $
        DatabaseDelete (MkTupleTableSel PinaforeLiteral) $ MkTupleWhereClause $ ColumnExpr LiteralKey === ConstExpr v
    clPutEdits ::
           forall m. MonadIO m
        => [PinaforeTableEdit]
        -> Readable m (SQLiteReader PinaforeSchema)
        -> m (Maybe [SQLiteEdit PinaforeSchema])
    clPutEdits = clPutEditsFromSimplePutEdit clPutEdit
    in MkChangeLens {..}

instance WitnessConstraint IsPinaforeRow PinaforeSchema where
    witnessConstraint PinaforeProperty = Dict
    witnessConstraint PinaforeModelCount = Dict
    witnessConstraint PinaforeFact = Dict
    witnessConstraint PinaforeLiteral = Dict

instance ShowableTupleDatabase SQLiteDatabase PinaforeSchema where
    witnessTupleRow = Dict

sqlitePinaforeTableReference :: FilePath -> IO (Reference PinaforeTableEdit)
sqlitePinaforeTableReference path = do
    obj <- sqliteReference path sqlitePinaforeSchema
    return $ mapReference sqlitePinaforeLens obj

sqlitePinaforeTableGetEntireDatabase :: ResourceContext -> FilePath -> IO (AllFor [] (TupleTableSel PinaforeSchema))
sqlitePinaforeTableGetEntireDatabase rc path = do
    obj <- sqliteReference path sqlitePinaforeSchema
    getReferenceSubject rc obj
