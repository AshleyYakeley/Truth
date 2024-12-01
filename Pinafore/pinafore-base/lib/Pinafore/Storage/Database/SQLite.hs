{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Storage.Database.SQLite
    ( sqliteTableReference
    , sqliteTableGetEntireDatabase
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
        maybeToOk $ decode serializeStrictCodec bs

instance ToField Anchor where
    toField = toField . encodeM serializeStrictCodec

instance FieldType Entity where
    fieldTypeName = fieldTypeName @Anchor

deriving newtype instance FromField Entity

deriving newtype instance ToField Entity

instance FieldType Predicate where
    fieldTypeName = fieldTypeName @Anchor

deriving newtype instance FromField Predicate

deriving newtype instance ToField Predicate

instance FieldType Literal where
    fieldTypeName = "BLOB"

instance FromField Literal where
    fromField f = do
        bs <- fromField f
        maybeToOk $ decode serializeStrictCodec bs

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

instance WitnessConstraint IsSQLiteTable QSchema where
    witnessConstraint QSProperty = Dict
    witnessConstraint QSModelCount = Dict
    witnessConstraint QSFact = Dict
    witnessConstraint QSLiteral = Dict

sqliteQSchema :: DatabaseSchema QSchema
sqliteQSchema = let
    databaseTables =
        mkFiniteAllFor @TableSchema $ \case
            QSProperty -> let
                tableName = "property"
                tableColumns =
                    mkFiniteAllFor @ColumnSchema $ \case
                        TriplePredicate -> MkColumnSchema "predicate" ColumnTypeNotNull True
                        TripleSubject -> MkColumnSchema "subject" ColumnTypeNotNull True
                        TripleValue -> MkColumnSchema "value" ColumnTypeNotNull False
                tableIndexes = [MkIndexSchema "propval" [MkSome TriplePredicate, MkSome TripleValue]]
                in MkTableSchema {..}
            QSModelCount -> let
                tableName = "refcount"
                tableColumns =
                    mkFiniteAllFor @ColumnSchema $ \case
                        RefCountKey -> MkColumnSchema "key" ColumnTypeNotNull True
                        RefCountValue -> MkColumnSchema "value" ColumnTypeNotNull False
                tableIndexes = []
                in MkTableSchema {..}
            QSFact -> let
                tableName = "fact"
                tableColumns =
                    mkFiniteAllFor @ColumnSchema $ \case
                        TriplePredicate -> MkColumnSchema "predicate" ColumnTypeNotNull True
                        TripleSubject -> MkColumnSchema "subject" ColumnTypeNotNull True
                        TripleValue -> MkColumnSchema "value" ColumnTypeNotNull False
                tableIndexes = [MkIndexSchema "factval" [MkSome TriplePredicate, MkSome TripleValue]]
                in MkTableSchema {..}
            QSLiteral -> let
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

instance TupleDatabase SQLiteDatabase QSchema where
    type TupleDatabaseRowWitness SQLiteDatabase QSchema = IsPinaforeRow

sqlitePinaforeLens :: ChangeLens (SQLiteUpdate QSchema) QTableUpdate
sqlitePinaforeLens = let
    clRead :: ReadFunction (SQLiteReader QSchema) QTableRead
    clRead mr (QTableReadPropertyGet p s) = do
        row <-
            mr $
            DatabaseSelect
                (SingleTable $ MkTupleTableSel QSProperty)
                (MkTupleWhereClause $
                 (ColumnExpr TriplePredicate === ConstExpr p) /\ (ColumnExpr TripleSubject === ConstExpr s))
                mempty
                (MkTupleSelectClause $ \Refl -> ColumnExpr TripleValue)
        return $ fmap getSingleAllOf $ listToMaybe row
    clRead mr (QTableReadPropertyLookup p v) = do
        row <-
            mr $
            DatabaseSelect
                (SingleTable $ MkTupleTableSel QSProperty)
                (MkTupleWhereClause $
                 (ColumnExpr TriplePredicate === ConstExpr p) /\ (ColumnExpr TripleValue === ConstExpr v))
                mempty
                (MkTupleSelectClause $ \Refl -> ColumnExpr TripleSubject)
        return $ MkFiniteSet $ fmap getSingleAllOf row
    clRead mr (QTableReadEntityRefCount v) = do
        (row :: [AllOf ((:~:) RefCount)]) <-
            mr $
            DatabaseSelect
                (SingleTable $ MkTupleTableSel QSModelCount)
                (MkTupleWhereClause $ ColumnExpr RefCountKey === ConstExpr v)
                mempty
                (MkTupleSelectClause $ \Refl -> ColumnExpr RefCountValue)
        return $ do
            sa <- listToMaybe row
            return $ getSingleAllOf sa
    clRead mr (QTableReadFactGet p s) = do
        row <-
            mr $
            DatabaseSelect
                (SingleTable $ MkTupleTableSel QSFact)
                (MkTupleWhereClause $
                 (ColumnExpr TriplePredicate === ConstExpr p) /\ (ColumnExpr TripleSubject === ConstExpr s))
                mempty
                (MkTupleSelectClause $ \Refl -> ColumnExpr TripleValue)
        return $ fmap getSingleAllOf $ listToMaybe row
    clRead mr (QTableReadLiteralGet v) = do
        (row :: [AllOf ((:~:) Literal)]) <-
            mr $
            DatabaseSelect
                (SingleTable $ MkTupleTableSel QSLiteral)
                (MkTupleWhereClause $ ColumnExpr LiteralKey === ConstExpr v)
                mempty
                (MkTupleSelectClause $ \Refl -> ColumnExpr LiteralValue)
        return $ do
            sa <- listToMaybe row
            return $ getSingleAllOf sa
    clUpdate ::
           forall m. MonadIO m
        => SQLiteUpdate QSchema
        -> Readable m (EditReader (SQLiteEdit QSchema))
        -> m [QTableUpdate]
    clUpdate _ _ = return $ error "sqlitePinaforeLens.editUpdate"
    clPutEdit ::
           forall m. MonadIO m
        => QTableEdit
        -> m (Maybe [SQLiteEdit QSchema])
    clPutEdit (QTableEditPropertySet p s (Just v)) =
        return $
        Just $
        pure $
        DatabaseInsert (MkTupleTableSel QSProperty) $
        MkTupleInsertClause $
        pure $
        MkAllOf $ \case
            TriplePredicate -> p
            TripleSubject -> s
            TripleValue -> v
    clPutEdit (QTableEditPropertySet p s Nothing) =
        return $
        Just $
        pure $
        DatabaseDelete (MkTupleTableSel QSProperty) $
        MkTupleWhereClause $ ColumnExpr TriplePredicate === ConstExpr p /\ ColumnExpr TripleSubject === ConstExpr s
    clPutEdit (QTableEditEntityRefCount v (Just rc)) =
        return $
        Just $
        pure $
        DatabaseInsert (MkTupleTableSel QSModelCount) $
        MkTupleInsertClause $
        pure $
        MkAllOf $ \case
            RefCountKey -> v
            RefCountValue -> rc
    clPutEdit (QTableEditEntityRefCount v Nothing) =
        return $
        Just $
        pure $
        DatabaseDelete (MkTupleTableSel QSModelCount) $ MkTupleWhereClause $ ColumnExpr RefCountKey === ConstExpr v
    clPutEdit (QTableEditFactSet p s (Just v)) =
        return $
        Just $
        pure $
        DatabaseInsert (MkTupleTableSel QSFact) $
        MkTupleInsertClause $
        pure $
        MkAllOf $ \case
            TriplePredicate -> p
            TripleSubject -> s
            TripleValue -> v
    clPutEdit (QTableEditFactSet p s Nothing) =
        return $
        Just $
        pure $
        DatabaseDelete (MkTupleTableSel QSFact) $
        MkTupleWhereClause $ ColumnExpr TriplePredicate === ConstExpr p /\ ColumnExpr TripleSubject === ConstExpr s
    clPutEdit (QTableEditLiteralSet v (Just l)) =
        return $
        Just $
        pure $
        DatabaseInsert (MkTupleTableSel QSLiteral) $
        MkTupleInsertClause $
        pure $
        MkAllOf $ \case
            LiteralKey -> v
            LiteralValue -> l
    clPutEdit (QTableEditLiteralSet v Nothing) =
        return $
        Just $
        pure $ DatabaseDelete (MkTupleTableSel QSLiteral) $ MkTupleWhereClause $ ColumnExpr LiteralKey === ConstExpr v
    clPutEdits ::
           forall m. MonadIO m
        => [QTableEdit]
        -> Readable m (SQLiteReader QSchema)
        -> m (Maybe [SQLiteEdit QSchema])
    clPutEdits = clPutEditsFromSimplePutEdit clPutEdit
    in MkChangeLens {..}

instance WitnessConstraint IsPinaforeRow QSchema where
    witnessConstraint QSProperty = Dict
    witnessConstraint QSModelCount = Dict
    witnessConstraint QSFact = Dict
    witnessConstraint QSLiteral = Dict

instance ShowableTupleDatabase SQLiteDatabase QSchema where
    witnessTupleRow = Dict

sqliteTableReference :: FilePath -> IO (Reference QTableEdit)
sqliteTableReference path = do
    obj <- sqliteReference path sqliteQSchema
    return $ mapReference sqlitePinaforeLens obj

sqliteTableGetEntireDatabase :: ResourceContext -> FilePath -> IO (AllFor [] (TupleTableSel QSchema))
sqliteTableGetEntireDatabase rc path = do
    obj <- sqliteReference path sqliteQSchema
    getReferenceSubject rc obj
