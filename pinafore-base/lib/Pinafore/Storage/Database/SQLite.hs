{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Storage.Database.SQLite
    ( sqlitePinaforeTableObject
    , sqlitePinaforeTableGetEntireDatabase
    ) where

import Data.UUID
import Pinafore.Base
import Pinafore.Storage.Database
import Pinafore.Storage.Table
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

instance FieldType Anchor where
    fieldTypeName = fieldTypeName @UUID

deriving instance FromField Anchor

deriving instance ToField Anchor

instance FieldType Entity where
    fieldTypeName = fieldTypeName @Anchor

deriving instance FromField Entity

deriving instance ToField Entity

instance FieldType Predicate where
    fieldTypeName = fieldTypeName @Anchor

deriving instance FromField Predicate

deriving instance ToField Predicate

instance FieldType Literal where
    fieldTypeName = fieldTypeName @Text

deriving instance FromField Literal

deriving instance ToField Literal

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

instance WitnessConstraint IsSQLiteTable PinaforeSchema where
    witnessConstraint PinaforeTriple = Dict
    witnessConstraint PinaforeLiteral = Dict

sqlitePinaforeSchema :: DatabaseSchema PinaforeSchema
sqlitePinaforeSchema = let
    databaseTables = let
        subWitnessDomain = [MkAnyW PinaforeTriple, MkAnyW PinaforeLiteral]
        subWitnessMap :: PinaforeSchema t -> TableSchema t
        subWitnessMap PinaforeTriple = let
            tableName = "triple"
            tableColumns = let
                domain = [MkAnyW TripleSubject, MkAnyW TriplePredicate, MkAnyW TripleValue]
                witmap :: TripleTable t -> ColumnSchema t
                witmap TriplePredicate = MkColumnSchema "predicate" ColumnTypeNotNull True
                witmap TripleSubject = MkColumnSchema "subject" ColumnTypeNotNull True
                witmap TripleValue = MkColumnSchema "value" ColumnTypeNotNull False
                in MkSubmapWitness domain witmap
            tableIndexes = [MkIndexSchema "predval" [MkAnyW TriplePredicate, MkAnyW TripleValue]]
            in MkTableSchema {..}
        subWitnessMap PinaforeLiteral = let
            tableName = "literal"
            tableColumns = let
                domain = [MkAnyW LiteralKey, MkAnyW LiteralValue]
                witmap :: LiteralTable t -> ColumnSchema t
                witmap LiteralKey = MkColumnSchema "key" ColumnTypeNotNull True
                witmap LiteralValue = MkColumnSchema "value" ColumnTypeNotNull False
                in MkSubmapWitness domain witmap
            tableIndexes = [MkIndexSchema "litval" [MkAnyW LiteralValue]]
            in MkTableSchema {..}
        in MkSubmapWitness {..}
    in MkDatabaseSchema {..}

class (FiniteWitness colsel, WitnessConstraint Show colsel, AllWitnessConstraint Show colsel) =>
          IsPinaforeRow (colsel :: Type -> Type)

instance (FiniteWitness colsel, WitnessConstraint Show colsel, AllWitnessConstraint Show colsel) => IsPinaforeRow colsel

instance TupleDatabase SQLiteDatabase PinaforeSchema where
    type TupleDatabaseRowWitness SQLiteDatabase PinaforeSchema = IsPinaforeRow

sqlitePinaforeLens :: EditLens (SQLiteUpdate PinaforeSchema) PinaforeTableUpdate
sqlitePinaforeLens = let
    ufGet :: ReadFunctionT IdentityT (SQLiteReader PinaforeSchema) PinaforeTableRead
    ufGet mr (PinaforeTableReadGetPredicate p s) =
        lift $ do
            row <-
                mr $
                DatabaseSelect
                    (SingleTable $ MkTupleTableSel PinaforeTriple)
                    (MkTupleWhereClause $
                     (ColumnExpr TriplePredicate === ConstExpr p) /\ (ColumnExpr TripleSubject === ConstExpr s))
                    mempty
                    (MkTupleSelectClause $ \Refl -> ColumnExpr TripleValue)
            return $ fmap getSingleAll $ listToMaybe row
    ufGet mr (PinaforeTableReadLookupPredicate p v) =
        lift $ do
            row <-
                mr $
                DatabaseSelect
                    (SingleTable $ MkTupleTableSel PinaforeTriple)
                    (MkTupleWhereClause $
                     (ColumnExpr TriplePredicate === ConstExpr p) /\ (ColumnExpr TripleValue === ConstExpr v))
                    mempty
                    (MkTupleSelectClause $ \Refl -> ColumnExpr TripleSubject)
            return $ MkFiniteSet $ fmap getSingleAll row
    ufGet mr (PinaforeTableReadGetLiteral v) =
        lift $ do
            (row :: [AllValue ((:~:) Literal)]) <-
                mr $
                DatabaseSelect
                    (SingleTable $ MkTupleTableSel PinaforeLiteral)
                    (MkTupleWhereClause $ ColumnExpr LiteralKey === ConstExpr v)
                    mempty
                    (MkTupleSelectClause $ \Refl -> ColumnExpr LiteralValue)
            return $ do
                sa <- listToMaybe row
                return $ getSingleAll sa
    ufUpdate ::
           forall m. MonadIO m
        => SQLiteUpdate PinaforeSchema
        -> MutableRead m (EditReader (SQLiteEdit PinaforeSchema))
        -> IdentityT m [PinaforeTableUpdate]
    ufUpdate _ _ = return $ error "sqlitePinaforeLens.editUpdate"
    elFunction :: AnUpdateFunction IdentityT (SQLiteUpdate PinaforeSchema) PinaforeTableUpdate
    elFunction = MkAnUpdateFunction {..}
    elPutEdit ::
           forall m. MonadIO m
        => PinaforeTableEdit
        -> IdentityT m (Maybe [SQLiteEdit PinaforeSchema])
    elPutEdit (PinaforeTableEditSetPredicate p s (Just v)) =
        return $
        Just $
        pure $
        DatabaseInsert (MkTupleTableSel PinaforeTriple) $
        MkTupleInsertClause $
        pure $
        MkAllValue $ \case
            TriplePredicate -> p
            TripleSubject -> s
            TripleValue -> v
    elPutEdit (PinaforeTableEditSetPredicate p s Nothing) =
        return $
        Just $
        pure $
        DatabaseDelete (MkTupleTableSel PinaforeTriple) $
        MkTupleWhereClause $ ColumnExpr TriplePredicate === ConstExpr p /\ ColumnExpr TripleSubject === ConstExpr s
    elPutEdit (PinaforeTableEditSetLiteral v (Just l)) =
        return $
        Just $
        pure $
        DatabaseInsert (MkTupleTableSel PinaforeLiteral) $
        MkTupleInsertClause $
        pure $
        MkAllValue $ \case
            LiteralKey -> v
            LiteralValue -> l
    elPutEdit (PinaforeTableEditSetLiteral v Nothing) =
        return $
        Just $
        pure $
        DatabaseDelete (MkTupleTableSel PinaforeLiteral) $ MkTupleWhereClause $ ColumnExpr LiteralKey === ConstExpr v
    elPutEdits ::
           forall m. MonadIO m
        => [PinaforeTableEdit]
        -> MutableRead m (SQLiteReader PinaforeSchema)
        -> IdentityT m (Maybe [SQLiteEdit PinaforeSchema])
    elPutEdits = elPutEditsFromSimplePutEdit elPutEdit
    in MkCloseUnlift wUnIdentityT $ MkAnEditLens {..}

instance WitnessConstraint FiniteWitness PinaforeSchema where
    witnessConstraint PinaforeTriple = Dict
    witnessConstraint PinaforeLiteral = Dict

instance WitnessConstraint (AllWitnessConstraint Show) PinaforeSchema where
    witnessConstraint PinaforeTriple = Dict
    witnessConstraint PinaforeLiteral = Dict

instance WitnessConstraint (WitnessConstraint Show) PinaforeSchema where
    witnessConstraint PinaforeTriple = Dict
    witnessConstraint PinaforeLiteral = Dict

instance WitnessConstraint IsPinaforeRow PinaforeSchema where
    witnessConstraint PinaforeTriple = Dict
    witnessConstraint PinaforeLiteral = Dict

instance ShowableTupleDatabase SQLiteDatabase PinaforeSchema where
    witnessTupleRow = Dict

sqlitePinaforeTableObject :: FilePath -> Object PinaforeTableEdit
sqlitePinaforeTableObject path = mapObject sqlitePinaforeLens $ sqliteObject path sqlitePinaforeSchema

sqlitePinaforeTableGetEntireDatabase :: FilePath -> IO (AllF (TupleTableSel PinaforeSchema) [])
sqlitePinaforeTableGetEntireDatabase path = getObjectSubject $ sqliteObject path sqlitePinaforeSchema
