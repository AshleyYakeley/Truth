{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE EmptyCase #-}
module Truth.World.Soup.SQLite(sqliteSoupObject) where
{
    import Truth.Core.Import;
    import Truth.Core;
    import Data.UUID;
    import Truth.World.SQLite;
    import Truth.World.SQLite.Schema;
    import Truth.World.Soup.Schema;


    instance FieldType UUID where {fieldTypeName="BLOB"};
    instance FromField UUID where
    {
        fromField f = do
        {
            bs <- fromField f;
            maybeToOk $ fromByteString bs;
        };
    };
    instance ToField UUID where
    {
        toField = toField . toByteString;
    };

    data TripleTable t where
    {
        TriplePredicate :: TripleTable UUID;
        TripleSubject :: TripleTable UUID;
        TripleValue :: TripleTable UUID;
    };

    instance FiniteWitness TripleTable where
    {
        assembleWitnessF getw = (\p s v -> MkAllF $ \case {TriplePredicate -> p;TripleSubject -> s;TripleValue -> v;}) <$> getw TriplePredicate <*> getw TripleSubject <*> getw TripleValue;
    };

    instance WitnessConstraint FromField TripleTable where
    {
        witnessConstraint TriplePredicate = MkConstraintWitness;
        witnessConstraint TripleSubject = MkConstraintWitness;
        witnessConstraint TripleValue = MkConstraintWitness;
    };

    instance WitnessConstraint ToField TripleTable where
    {
        witnessConstraint TriplePredicate = MkConstraintWitness;
        witnessConstraint TripleSubject = MkConstraintWitness;
        witnessConstraint TripleValue = MkConstraintWitness;
    };

    data LiteralTable t where
    {
        LiteralKey :: LiteralTable UUID;
        LiteralValue :: LiteralTable ByteString;
    };

    instance FiniteWitness LiteralTable where
    {
        assembleWitnessF getw = (\k v -> MkAllF $ \case {LiteralKey -> k;LiteralValue -> v;}) <$> getw LiteralKey <*> getw LiteralValue;
    };

    instance WitnessConstraint FromField LiteralTable where
    {
        witnessConstraint LiteralKey = MkConstraintWitness;
        witnessConstraint LiteralValue = MkConstraintWitness;
    };

    instance WitnessConstraint ToField LiteralTable where
    {
        witnessConstraint LiteralKey = MkConstraintWitness;
        witnessConstraint LiteralValue = MkConstraintWitness;
    };

    data SoupSchema colsel where
    {
        SoupTriple :: SoupSchema TripleTable;
        SoupLiteral :: SoupSchema LiteralTable;
    };

    instance TestEquality SoupSchema where
    {
        testEquality SoupTriple SoupTriple = Just Refl;
        testEquality SoupLiteral SoupLiteral = Just Refl;
        testEquality _ _ = Nothing;
    };

    instance FiniteWitness SoupSchema where
    {
        assembleWitnessF getTable = (\ft fl -> MkAllF $ \case {SoupTriple -> ft; SoupLiteral -> fl;}) <$> getTable SoupTriple <*> getTable SoupLiteral;
    };

    instance WitnessConstraint IsSQLiteTable SoupSchema where
    {
        witnessConstraint SoupTriple = MkConstraintWitness;
        witnessConstraint SoupLiteral = MkConstraintWitness;
    };

    soupSchema :: DatabaseSchema SoupSchema;
    soupSchema = let
    {
        databaseTables = let
        {
            subWitnessDomain = [MkAnyWitness SoupTriple,MkAnyWitness SoupLiteral];
            subWitnessMap :: SoupSchema t -> TableSchema t;
            subWitnessMap SoupTriple = let
            {
                tableName = "triple";
                tableColumns = let
                {
                    domain = [MkAnyWitness TripleSubject,MkAnyWitness TriplePredicate,MkAnyWitness TripleValue];
                    witmap :: TripleTable t -> ColumnSchema t;
                    witmap TriplePredicate = MkColumnSchema "predicate" ColumnTypeNotNull True;
                    witmap TripleSubject = MkColumnSchema "subject" ColumnTypeNotNull True;
                    witmap TripleValue = MkColumnSchema "value" ColumnTypeNotNull False;
                } in MkSubmapWitness domain witmap;
                tableIndexes = [MkIndexSchema "predval" [MkAnyWitness TriplePredicate,MkAnyWitness TripleValue]];
            } in MkTableSchema{..};
            subWitnessMap SoupLiteral = let
            {
                tableName = "literal-int";
                tableColumns = let
                {
                    domain = [MkAnyWitness LiteralKey,MkAnyWitness LiteralValue];
                    witmap :: LiteralTable t -> ColumnSchema t;
                    witmap LiteralKey = MkColumnSchema "key" ColumnTypeNotNull True;
                    witmap LiteralValue = MkColumnSchema "value" ColumnTypeNotNull False;
                } in MkSubmapWitness domain witmap;
                tableIndexes = [MkIndexSchema "litval" [MkAnyWitness LiteralValue]];
            } in MkTableSchema{..};
        } in MkSubmapWitness{..};
    } in MkDatabaseSchema{..};

    soupDatabaseLens :: PureEditLens' Identity () (SQLiteEdit SoupSchema) SoupEdit;
    soupDatabaseLens = let
    {
        editInitial = ();

        editGet :: () -> PureReadFunction (SQLiteRead SoupSchema) SoupRead;
        editGet () (SoupReadGetValue p s) = do
        {
            row <- readable $ DatabaseSelect
                (SingleTable $ MkTupleTableSel SoupTriple)
                (MkTupleWhereClause $ (ColumnExpr TriplePredicate === ConstExpr p) /\ (ColumnExpr TripleSubject === ConstExpr s))
                mempty
                (MkTupleSelectClause $ \Refl -> ColumnExpr TripleValue);
            return $ fmap getSingleAll $ listToMaybe row;
        };
        editGet () (SoupReadLookupValue p v) = do
        {
            row <- readable $ DatabaseSelect
                (SingleTable $ MkTupleTableSel SoupTriple)
                (MkTupleWhereClause $ (ColumnExpr TriplePredicate === ConstExpr p) /\ (ColumnExpr TripleValue === ConstExpr v))
                mempty
                (MkTupleSelectClause $ \Refl -> ColumnExpr TripleSubject);
            return $ MkFiniteSet $ fmap getSingleAll row;
        };
        editGet () (SoupReadGetPrimitive v) = do
        {
            (row :: [All ((:~:) ByteString)]) <- readable $ DatabaseSelect
                (SingleTable $ MkTupleTableSel SoupLiteral)
                (MkTupleWhereClause $ ColumnExpr LiteralKey === ConstExpr v)
                mempty
                (MkTupleSelectClause $ \Refl -> ColumnExpr LiteralValue);
            return $ do
            {
                sa <- listToMaybe row;
                decodeMaybe serializeCodec $ getSingleAll sa;
            };
        };
        editGet () (SoupReadLookupPrimitive p l) = do
        {
            row <- readable $ DatabaseSelect
                (JoinTables OuterTupleJoinClause (SingleTable $ MkTupleTableSel SoupTriple) (SingleTable $ MkTupleTableSel SoupLiteral))
                (MkTupleWhereClause $ (ColumnExpr (LeftWitness TriplePredicate) === ConstExpr p) /\ (ColumnExpr (LeftWitness TripleValue) === ColumnExpr (RightWitness LiteralKey)) /\ (ColumnExpr (RightWitness LiteralValue) === ConstExpr (encode serializeCodec l)))
                mempty
                (MkTupleSelectClause $ \Refl -> ColumnExpr (LeftWitness TripleSubject));
            return $ MkFiniteSet $ fmap getSingleAll row;
        };

        editUpdate _ _ = return undefined;

        editLensPutEdit :: () -> SoupEdit -> PureReadable (SQLiteRead SoupSchema) (Identity ((), [SQLiteEdit SoupSchema]));
        editLensPutEdit () (SoupEditSetValue p s (Just v)) = pure $ pure $ pure $ pure $ DatabaseInsert (MkTupleTableSel SoupTriple) $ MkTupleInsertClause $ pure $ MkAll $ \case
        {
            TriplePredicate -> p;
            TripleSubject -> s;
            TripleValue -> v;
        };
        editLensPutEdit () (SoupEditSetValue p s Nothing) = pure $ pure $ pure $ pure $ DatabaseDelete (MkTupleTableSel SoupTriple) $ MkTupleWhereClause $ ColumnExpr TriplePredicate === ConstExpr p /\ ColumnExpr TripleSubject === ConstExpr s;
        editLensPutEdit () (SoupEditDeleteTriple p s v) = pure $ pure $ pure $ pure $ DatabaseDelete (MkTupleTableSel SoupTriple) $ MkTupleWhereClause $ ColumnExpr TriplePredicate === ConstExpr p /\ ColumnExpr TripleSubject === ConstExpr s /\ ColumnExpr TripleValue === ConstExpr v;
        editLensPutEdit () (SoupEditDeleteLookupValue p v) = pure $ pure $ pure $ pure $ DatabaseDelete (MkTupleTableSel SoupTriple) $ MkTupleWhereClause $ ColumnExpr TriplePredicate === ConstExpr p /\ ColumnExpr TripleValue === ConstExpr v;
        editLensPutEdit () (SoupEditSetPrimitive v (Just l)) = pure $ pure $ pure $ pure $ DatabaseInsert (MkTupleTableSel SoupLiteral) $ MkTupleInsertClause $ pure $ MkAll $ \case
        {
            LiteralKey -> v;
            LiteralValue -> encode serializeCodec l;
        };
        editLensPutEdit () (SoupEditSetPrimitive v Nothing) = pure $ pure $ pure $ pure $ DatabaseDelete (MkTupleTableSel SoupLiteral) $ MkTupleWhereClause $ ColumnExpr LiteralKey === ConstExpr v;

        editLensFunction = MkEditFunction{..};
    } in MkEditLens{..};

    sqliteSoupObject :: FilePath -> Object SoupEdit;
    sqliteSoupObject path = pureFixedMapObject soupDatabaseLens $ sqliteObject path soupSchema;
}
