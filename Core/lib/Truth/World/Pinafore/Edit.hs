module Truth.World.Pinafore.Edit where
{
    import Truth.Core.Import;
    import Truth.Core;
    import Data.UUID;
    import Data.UUID.V4 as UUID;


    data PinaforeRead t where
    {
        PinaforeReadGetValue :: UUID -> UUID -> PinaforeRead (Maybe UUID);
        PinaforeReadLookupValue :: UUID -> UUID -> PinaforeRead (FiniteSet UUID);
        PinaforeReadGetPrimitive :: Serialize t => UUID -> PinaforeRead (Maybe t);
        PinaforeReadLookupPrimitive :: Serialize t => UUID -> t -> PinaforeRead (FiniteSet UUID);
    };

    data PinaforeEdit where
    {
        PinaforeEditSetValue :: UUID -> UUID -> Maybe UUID -> PinaforeEdit; -- pred subj mval
        PinaforeEditDeleteTriple :: UUID -> UUID -> UUID -> PinaforeEdit; -- pred subj val -- delete this triple if it exists
        PinaforeEditDeleteLookupValue :: UUID -> UUID -> PinaforeEdit; -- pred val -- delete all triples matching pred val
        PinaforeEditSetPrimitive :: Serialize t => UUID -> Maybe t -> PinaforeEdit;
    };

    instance Reader PinaforeRead where
    {
        type ReaderSubject PinaforeRead = ([(UUID,UUID,UUID)],[(UUID,ByteString)]);

        readFrom (triples,_) (PinaforeReadGetValue rp rs) = listToMaybe $ [v | (p,s,v) <- triples,p == rp && s == rs];
        readFrom (triples,_) (PinaforeReadLookupValue rp rv) = MkFiniteSet [s | (p,s,v) <- triples, p == rp, v == rv];
        readFrom (_,literals) (PinaforeReadGetPrimitive rv) = do
        {
            d <- listToMaybe [l | (v,l) <- literals, v == rv];
            decodeMaybe serializeCodec d;
        };
        readFrom (triples,literals) (PinaforeReadLookupPrimitive rp rl) = MkFiniteSet [s | (p,s,v) <- triples, rp == p, (v',l) <- literals, v == v', l == encode serializeCodec rl];
    };

    instance Floating PinaforeEdit PinaforeEdit;
    instance Edit PinaforeEdit where
    {
        type EditReader PinaforeEdit = PinaforeRead;
        applyEdit _ _ = return undefined;
        invertEdit _ = return undefined;
    };

    soupPrimitiveLens :: forall t. Serialize t => UUID -> PureEditLens' Identity () PinaforeEdit (WholeEdit (Maybe t));
    soupPrimitiveLens valkey = let
    {
        editInitial = ();

        editGet :: () -> WholeReader (Maybe t) a -> PureReadable PinaforeRead a;
        editGet () ReadWhole = readable $ PinaforeReadGetPrimitive valkey;

        editUpdate :: PinaforeEdit -> () -> PureReadable PinaforeRead ((),[WholeEdit (Maybe t)]);
        editUpdate (PinaforeEditSetPrimitive k mt) () | k == valkey = pure $ pure $ pure $ MkWholeEdit $ mt >>= decodeMaybe serializeCodec . encode serializeCodec;
        editUpdate _ () = pure $ pure [];

        editLensFunction = MkEditFunction{..};

        editLensPutEdit :: () -> WholeEdit (Maybe t) -> PureReadable PinaforeRead (Identity ((),[PinaforeEdit]));
        editLensPutEdit () (MkWholeEdit mt) = pure $ pure $ pure $ pure $ PinaforeEditSetPrimitive valkey mt;
    } in MkEditLens{..};

    soupTripleLens :: UUID -> UUID -> PureEditLens () PinaforeEdit (WholeEdit (Maybe UUID));
    soupTripleLens prd subj = let
    {
        editInitial = ();

        editGet :: () -> WholeReader (Maybe UUID) a -> PureReadable PinaforeRead a;
        editGet () ReadWhole = readable $ PinaforeReadGetValue prd subj;

        editUpdate :: PinaforeEdit -> () -> PureReadable PinaforeRead ((),[WholeEdit (Maybe UUID)]);
        editUpdate (PinaforeEditSetValue p s mv) () | p == prd && s == subj = pure $ pure $ pure $ MkWholeEdit mv;
        editUpdate _ () = pure $ pure [];

        editLensFunction = MkEditFunction{..};

        editLensPutEdit :: () -> WholeEdit (Maybe UUID) -> PureReadable PinaforeRead (Maybe ((),[PinaforeEdit]));
        editLensPutEdit () (MkWholeEdit mv) = pure $ pure $ pure $ pure $ PinaforeEditSetValue prd subj mv;
    } in MkEditLens{..};

    type PinaforeMorphism = PointedEditLens MonadIO Maybe PinaforeEdit;

    predicatePinaforeMorphism :: UUID -> PinaforeMorphism (WholeEdit (Maybe UUID)) (WholeEdit (Maybe UUID));
    predicatePinaforeMorphism prd = MkPointedEditLens $ let
    {
        editInitial = ();

        editGet :: forall t. () -> WholeReader (Maybe UUID) t -> IOReadable (ContextEditReader PinaforeEdit (WholeEdit (Maybe UUID))) t;
        editGet () ReadWhole = do
        {
            msubj <- readable $ MkTupleEditReader EditContent ReadWhole;
            case msubj of
            {
                Just subj -> readable $ MkTupleEditReader EditContext $ PinaforeReadGetValue prd subj;
                Nothing -> return Nothing;
            };
        };

        editUpdate :: (ContextEdit PinaforeEdit (WholeEdit (Maybe UUID))) -> () -> IOReadable (ContextEditReader PinaforeEdit (WholeEdit (Maybe UUID))) ((),[WholeEdit (Maybe UUID)]);
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

        editLensPutEdit :: () -> WholeEdit (Maybe UUID) -> IOReadable (ContextEditReader PinaforeEdit (WholeEdit (Maybe UUID))) (Maybe ((),[ContextEdit PinaforeEdit (WholeEdit (Maybe UUID))]));
        editLensPutEdit () (MkWholeEdit mv) = do
        {
            msubj <- readable $ MkTupleEditReader EditContent ReadWhole;
            case msubj of
            {
                Just subj -> return $ pure $ pure [MkTupleEdit EditContext $ PinaforeEditSetValue prd subj mv];
                Nothing -> do
                {
                    subj <- liftIO UUID.nextRandom;
                    return $ pure $ pure [MkTupleEdit EditContent $ MkWholeEdit $ Just subj, MkTupleEdit EditContext $ PinaforeEditSetValue prd subj mv];
                };
            };
        };
    } in MkEditLens{..};

    predicateInversePinaforeMorphism :: UUID -> PinaforeMorphism (WholeEdit (Maybe UUID)) (FiniteSetEdit UUID);
    predicateInversePinaforeMorphism prd = MkPointedEditLens $ let
    {
        editInitial = ();

        editGet :: forall t. () -> FiniteSetReader UUID t -> IOReadable (ContextEditReader PinaforeEdit (WholeEdit (Maybe UUID))) t;
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

        editUpdate :: ContextEdit PinaforeEdit (WholeEdit (Maybe UUID)) -> () -> IOReadable (ContextEditReader PinaforeEdit (WholeEdit (Maybe UUID))) ((), [FiniteSetEdit UUID]);
        editUpdate (MkTupleEdit EditContext (PinaforeEditSetValue p s mnewv)) () | p == prd = do
        {
            mval <- readable $ MkTupleEditReader EditContent ReadWhole;
            case mval of
            {
                Just val -> do
                {
                    moldv <- readable $ MkTupleEditReader EditContext $ PinaforeReadGetValue p s;
                    return $ pure $ case (Just val == moldv,Just val == mnewv) of
                    {
                        (True,False) -> [KeyDeleteItem s];
                        (False,True) -> [KeyInsertReplaceItem s];
                        _ -> [];
                    };
                };
                Nothing -> return $ pure [];
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
            edits <- getReplaceEditsM @MonadIO subjs;
            return $ pure edits;
        };

        editLensFunction :: IOEditFunction () (ContextEdit PinaforeEdit (WholeEdit (Maybe UUID))) (FiniteSetEdit UUID);
        editLensFunction = MkEditFunction{..};

        editLensPutEdit :: () -> FiniteSetEdit UUID -> IOReadable (ContextEditReader PinaforeEdit (WholeEdit (Maybe UUID))) (Maybe ((), [ContextEdit PinaforeEdit (WholeEdit (Maybe UUID))]));
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
                    val <- liftIO UUID.nextRandom;
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
