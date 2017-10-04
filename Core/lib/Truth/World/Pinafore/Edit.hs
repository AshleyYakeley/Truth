module Truth.World.Pinafore.Edit where
{
    import Truth.Core.Import;
    import Truth.Core;
    import Data.UUID;
    import Data.Serialize as Serialize(Serialize(..));
    import Data.Aeson (FromJSON);


    newtype Predicate = MkPredicate UUID deriving (Eq,FromJSON);
    newtype Point = MkPoint UUID deriving (Eq,Random,FromJSON);

    instance Serialize Point where
    {
        put (MkPoint uuid) = Serialize.put (toByteString uuid);
        get = do
        {
            bs <- Serialize.get;
            case fromByteString bs of
            {
                Just uuid -> return $ MkPoint uuid;
                Nothing -> fail "deserialize bad UUID";
            };
        };
    };

    data PinaforeRead t where
    {
        PinaforeReadGetValue :: Predicate -> Point -> PinaforeRead (Maybe Point);
        PinaforeReadLookupValue :: Predicate -> Point -> PinaforeRead (FiniteSet Point);
        PinaforeReadGetPrimitive :: Point -> PinaforeRead (Maybe ByteString);
        PinaforeReadLookupPrimitive :: Predicate -> ByteString -> PinaforeRead (FiniteSet Point);
    };

    data PinaforeEdit where
    {
        PinaforeEditSetValue :: Predicate -> Point -> Maybe Point -> PinaforeEdit; -- pred subj mval
        PinaforeEditDeleteTriple :: Predicate -> Point -> Point -> PinaforeEdit; -- pred subj val -- delete this triple if it exists
        PinaforeEditDeleteLookupValue :: Predicate -> Point -> PinaforeEdit; -- pred val -- delete all triples matching pred val
        PinaforeEditSetPrimitive :: Point -> Maybe ByteString -> PinaforeEdit;
    };

    instance SubjectReader PinaforeRead where
    {
        type ReaderSubject PinaforeRead = ([(Predicate,Point,Point)],[(Point,ByteString)]);

        readFromSubject (triples,_) (PinaforeReadGetValue rp rs) = listToMaybe $ [v | (p,s,v) <- triples,p == rp && s == rs];
        readFromSubject (triples,_) (PinaforeReadLookupValue rp rv) = MkFiniteSet [s | (p,s,v) <- triples, p == rp, v == rv];
        readFromSubject (_,literals) (PinaforeReadGetPrimitive rv) = do
        {
            d <- listToMaybe [l | (v,l) <- literals, v == rv];
            decodeMaybe serializeCodec d;
        };
        readFromSubject (triples,literals) (PinaforeReadLookupPrimitive rp rl) = MkFiniteSet [s | (p,s,v) <- triples, rp == p, (v',l) <- literals, v == v', l == encode serializeCodec rl];
    };

    instance Floating PinaforeEdit PinaforeEdit;
    instance Edit PinaforeEdit where
    {
        type EditReader PinaforeEdit = PinaforeRead;
        applyEdit _ _ = return undefined;
    };

    type PinaforeValue = GeneralLens PinaforeEdit;
    type PinaforeEditLens edita editb = PinaforeValue edita -> PinaforeValue editb;
    type PinaforeLens a b = PinaforeEditLens (WholeEdit (Maybe a)) (WholeEdit (Maybe b));
    type PinaforeInverseLens a b = PinaforeEditLens (WholeEdit (Maybe a)) (FiniteSetEdit b);

    primitivePinaforeLens :: forall val. Serialize val => PinaforeLens Point val;
    primitivePinaforeLens = pointedMapGeneralLens $ MkPointedEditLens $ let
    {
        editAccess :: IOStateAccess ();
        editAccess = unitStateAccess;

        editGet :: forall t. () -> WholeReader (Maybe val) t -> Readable (ContextEditReader PinaforeEdit (WholeEdit (Maybe Point))) t;
        editGet () ReadWhole = do
        {
            msubj <- readable $ MkTupleEditReader EditContent ReadWhole;
            case msubj of
            {
                Just subj -> do
                {
                    mbs <- readable $ MkTupleEditReader EditContext $ PinaforeReadGetPrimitive subj;
                    return $ mbs >>= decodeMaybe serializeCodec;
                };
                Nothing -> return Nothing;
            };
        };

        editUpdate :: (ContextEdit PinaforeEdit (WholeEdit (Maybe Point))) -> () -> Readable (ContextEditReader PinaforeEdit (WholeEdit (Maybe Point))) ((),[WholeEdit (Maybe val)]);
        editUpdate (MkTupleEdit EditContext (PinaforeEditSetPrimitive s mbs)) () = do
        {
            msubj <- readable $ MkTupleEditReader EditContent ReadWhole;
            return $ pure $ if Just s == msubj then [MkWholeEdit $ mbs >>= decodeMaybe serializeCodec] else [];
        };
        editUpdate (MkTupleEdit EditContext _) () = return $ pure [];
        editUpdate (MkTupleEdit EditContent (MkWholeEdit Nothing)) () = return $ pure [MkWholeEdit Nothing];
        editUpdate (MkTupleEdit EditContent (MkWholeEdit (Just subj))) () = do
        {
            mbs <- readable $ MkTupleEditReader EditContext $ PinaforeReadGetPrimitive subj;
            return $ pure $ [MkWholeEdit $ mbs >>= decodeMaybe serializeCodec];
        };

        editLensFunction = MkEditFunction{..};

        editLensPutEdit :: () -> WholeEdit (Maybe val) -> Readable (ContextEditReader PinaforeEdit (WholeEdit (Maybe Point))) (Maybe ((),[ContextEdit PinaforeEdit (WholeEdit (Maybe Point))]));
        editLensPutEdit () (MkWholeEdit (fmap (encode serializeCodec) -> mbs)) = do
        {
            msubj <- readable $ MkTupleEditReader EditContent ReadWhole;
            case msubj of
            {
                Just subj -> return $ pure $ pure [MkTupleEdit EditContext $ PinaforeEditSetPrimitive subj mbs];
                Nothing -> do
                {
                    subj <- liftIO randomIO;
                    return $ pure $ pure [MkTupleEdit EditContent $ MkWholeEdit $ Just subj, MkTupleEdit EditContext $ PinaforeEditSetPrimitive subj mbs];
                };
            };
        };

    } in MkEditLens{..};

    predicatePinaforeLens :: Predicate -> PinaforeLens Point Point;
    predicatePinaforeLens prd = pointedMapGeneralLens $ MkPointedEditLens $ let
    {
        editAccess :: IOStateAccess ();
        editAccess = unitStateAccess;

        editGet :: forall t. () -> WholeReader (Maybe Point) t -> Readable (ContextEditReader PinaforeEdit (WholeEdit (Maybe Point))) t;
        editGet () ReadWhole = do
        {
            msubj <- readable $ MkTupleEditReader EditContent ReadWhole;
            case msubj of
            {
                Just subj -> readable $ MkTupleEditReader EditContext $ PinaforeReadGetValue prd subj;
                Nothing -> return Nothing;
            };
        };

        editUpdate :: (ContextEdit PinaforeEdit (WholeEdit (Maybe Point))) -> () -> Readable (ContextEditReader PinaforeEdit (WholeEdit (Maybe Point))) ((),[WholeEdit (Maybe Point)]);
        editUpdate (MkTupleEdit EditContext (PinaforeEditSetValue p s mv)) () | p == prd = do
        {
            msubj <- readable $ MkTupleEditReader EditContent ReadWhole;
            return $ pure $ if Just s == msubj then [MkWholeEdit mv] else [];
        };
        editUpdate (MkTupleEdit EditContext (PinaforeEditDeleteLookupValue p v)) () | p == prd = do
        {
            msubj <- readable $ MkTupleEditReader EditContent ReadWhole;
            case msubj of
            {
                Just subj -> do
                {
                    mval <- readable $ MkTupleEditReader EditContext $ PinaforeReadGetValue p subj;
                    return $ pure $ if mval == Just v then [MkWholeEdit Nothing] else [];
                };
                Nothing -> return $ pure [];
            };
        };
        editUpdate (MkTupleEdit EditContext (PinaforeEditDeleteTriple p s v)) () | p == prd = do
        {
            msubj <- readable $ MkTupleEditReader EditContent ReadWhole;
            case msubj of
            {
                Just subj | subj == s -> do
                {
                    mval <- readable $ MkTupleEditReader EditContext $ PinaforeReadGetValue p subj;
                    return $ pure $ if mval == Just v then [MkWholeEdit Nothing] else [];
                };
                _ -> return $ pure [];
            };
        };
        editUpdate (MkTupleEdit EditContext _) () = return $ pure [];
        editUpdate (MkTupleEdit EditContent (MkWholeEdit Nothing)) () = return $ pure [MkWholeEdit Nothing];
        editUpdate (MkTupleEdit EditContent (MkWholeEdit (Just subj))) () = do
        {
            mval <- readable $ MkTupleEditReader EditContext $ PinaforeReadGetValue prd subj;
            return $ pure $ [MkWholeEdit mval];
        };

        editLensFunction = MkEditFunction{..};

        editLensPutEdit :: () -> WholeEdit (Maybe Point) -> Readable (ContextEditReader PinaforeEdit (WholeEdit (Maybe Point))) (Maybe ((),[ContextEdit PinaforeEdit (WholeEdit (Maybe Point))]));
        editLensPutEdit () (MkWholeEdit mv) = do
        {
            msubj <- readable $ MkTupleEditReader EditContent ReadWhole;
            case msubj of
            {
                Just subj -> return $ pure $ pure [MkTupleEdit EditContext $ PinaforeEditSetValue prd subj mv];
                Nothing -> do
                {
                    subj <- liftIO randomIO;
                    return $ pure $ pure [MkTupleEdit EditContent $ MkWholeEdit $ Just subj, MkTupleEdit EditContext $ PinaforeEditSetValue prd subj mv];
                };
            };
        };
    } in MkEditLens{..};

    predicateInversePinaforeLens :: Predicate -> PinaforeInverseLens Point Point;
    predicateInversePinaforeLens prd = pointedMapGeneralLens $ MkPointedEditLens $ let
    {
        editAccess :: IOStateAccess ();
        editAccess = unitStateAccess;

        editGet :: forall t. () -> FiniteSetReader Point t -> Readable (ContextEditReader PinaforeEdit (WholeEdit (Maybe Point))) t;
        editGet () KeyReadKeys = do
        {
            mval <- readable $ MkTupleEditReader EditContent ReadWhole;
            case mval of
            {
                Just val -> readable $ MkTupleEditReader EditContext $ PinaforeReadLookupValue prd val;
                Nothing -> return mempty;
            };
        };
        editGet () (KeyReadItem subj ReadWhole) = do
        {
            mval <- readable $ MkTupleEditReader EditContent ReadWhole;
            case mval of
            {
                Just _ -> do
                {
                    val' <- readable $ MkTupleEditReader EditContext $ PinaforeReadGetValue prd subj;
                    return $ if mval == val' then Just subj else Nothing;
                };
                Nothing -> return Nothing;
            };
        };

        editUpdate :: ContextEdit PinaforeEdit (WholeEdit (Maybe Point)) -> () -> Readable (ContextEditReader PinaforeEdit (WholeEdit (Maybe Point))) ((), [FiniteSetEdit Point]);
        editUpdate (MkTupleEdit EditContext (PinaforeEditSetValue p s mnewv)) () | p == prd = do
        {
            mval <- readable $ MkTupleEditReader EditContent ReadWhole;
            return $ pure $ case mval of
            {
                Just val | Just val == mnewv -> [KeyInsertReplaceItem s];
                Just _  -> [KeyDeleteItem s];
                Nothing -> [];
            };
        };
        editUpdate (MkTupleEdit EditContext (PinaforeEditDeleteLookupValue p v)) () | p == prd = do
        {
            mval <- readable $ MkTupleEditReader EditContent ReadWhole;
            return $ pure $ if mval == Just v then [KeyClear] else [];
        };
        editUpdate (MkTupleEdit EditContext (PinaforeEditDeleteTriple p s v)) () | p == prd = do
        {
            mval <- readable $ MkTupleEditReader EditContent ReadWhole;
            return $ pure $ if mval == Just v then [KeyDeleteItem s] else [];
        };
        editUpdate (MkTupleEdit EditContext _) () = return $ pure [];
        editUpdate (MkTupleEdit EditContent (MkWholeEdit Nothing)) () = return $ pure [KeyClear];
        editUpdate (MkTupleEdit EditContent (MkWholeEdit (Just val))) () = do
        {
            subjs <- readable $ MkTupleEditReader EditContext $ PinaforeReadLookupValue prd val;
            edits <- getReplaceEditsM subjs;
            return $ pure edits;
        };

        editLensFunction :: EditFunction () (ContextEdit PinaforeEdit (WholeEdit (Maybe Point))) (FiniteSetEdit Point);
        editLensFunction = MkEditFunction{..};

        editLensPutEdit :: () -> FiniteSetEdit Point -> Readable (ContextEditReader PinaforeEdit (WholeEdit (Maybe Point))) (Maybe ((), [ContextEdit PinaforeEdit (WholeEdit (Maybe Point))]));
        editLensPutEdit () (KeyEditItem _ edit) = never edit;
        editLensPutEdit () (KeyDeleteItem subj) = do
        {
            mval <- readable $ MkTupleEditReader EditContent ReadWhole;
            case mval of
            {
                Just val -> return $ Just $ pure $ [MkTupleEdit EditContext $ PinaforeEditDeleteTriple prd subj val];
                Nothing -> return $ Just $ pure [];
            };
        };
        editLensPutEdit () (KeyInsertReplaceItem subj) = do
        {
            mval <- readable $ MkTupleEditReader EditContent ReadWhole;
            case mval of
            {
                Just val -> return $ Just $ pure [MkTupleEdit EditContext $ PinaforeEditSetValue prd subj $ Just val];
                Nothing -> do
                {
                    val <- liftIO randomIO;
                    return $ Just $ pure [MkTupleEdit EditContent $ MkWholeEdit $ Just val, MkTupleEdit EditContext $ PinaforeEditSetValue prd subj $ Just val];
                };
            };
        };
        editLensPutEdit () KeyClear = do
        {
            mval <- readable $ MkTupleEditReader EditContent ReadWhole;
            case mval of
            {
                Just val -> return $ Just $ pure [MkTupleEdit EditContext $ PinaforeEditDeleteLookupValue prd val];
                Nothing -> return $ Just $ pure [];
            };
        };
    } in MkEditLens{..};
}
