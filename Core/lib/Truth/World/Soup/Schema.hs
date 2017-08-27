module Truth.World.Soup.Schema where
{
    import Truth.Core.Import;
    import Truth.Core;
    import Data.UUID;
    import Data.UUID.V4 as UUID;


    data SoupRead t where
    {
        SoupReadGetValue :: UUID -> UUID -> SoupRead (Maybe UUID);
        SoupReadLookupValue :: UUID -> UUID -> SoupRead (FiniteSet UUID);
        SoupReadGetPrimitive :: Serialize t => UUID -> SoupRead (Maybe t);
        SoupReadLookupPrimitive :: Serialize t => UUID -> t -> SoupRead (FiniteSet UUID);
    };

    data SoupEdit where
    {
        SoupEditSetValue :: UUID -> UUID -> Maybe UUID -> SoupEdit; -- pred subj mval
        SoupEditDeleteTriple :: UUID -> UUID -> UUID -> SoupEdit; -- pred subj val -- delete this triple if it exists
        SoupEditDeleteLookupValue :: UUID -> UUID -> SoupEdit; -- pred val -- delete all triples matching pred val
        SoupEditSetPrimitive :: Serialize t => UUID -> Maybe t -> SoupEdit;
    };

    instance Reader SoupRead where
    {
        type ReaderSubject SoupRead = ([(UUID,UUID,UUID)],[(UUID,ByteString)]);

        readFrom (triples,_) (SoupReadGetValue rp rs) = listToMaybe $ [v | (p,s,v) <- triples,p == rp && s == rs];
        readFrom (triples,_) (SoupReadLookupValue rp rv) = MkFiniteSet [s | (p,s,v) <- triples, p == rp, v == rv];
        readFrom (_,literals) (SoupReadGetPrimitive rv) = do
        {
            d <- listToMaybe [l | (v,l) <- literals, v == rv];
            decodeMaybe serializeCodec d;
        };
        readFrom (triples,literals) (SoupReadLookupPrimitive rp rl) = MkFiniteSet [s | (p,s,v) <- triples, rp == p, (v',l) <- literals, v == v', l == encode serializeCodec rl];
    };

    instance Floating SoupEdit SoupEdit;
    instance Edit SoupEdit where
    {
        type EditReader SoupEdit = SoupRead;
        applyEdit _ _ = return undefined;
        invertEdit _ = return undefined;
    };

    soupPrimitiveLens :: forall t. Serialize t => UUID -> PureEditLens' Identity () SoupEdit (WholeEdit (Maybe t));
    soupPrimitiveLens valkey = let
    {
        editInitial = ();

        editGet :: () -> WholeReader (Maybe t) a -> PureReadable SoupRead a;
        editGet () ReadWhole = readable $ SoupReadGetPrimitive valkey;

        editUpdate :: SoupEdit -> () -> PureReadable SoupRead ((),[WholeEdit (Maybe t)]);
        editUpdate (SoupEditSetPrimitive k mt) () | k == valkey = pure $ pure $ pure $ MkWholeEdit $ mt >>= decodeMaybe serializeCodec . encode serializeCodec;
        editUpdate _ () = pure $ pure [];

        editLensFunction = MkEditFunction{..};

        editLensPutEdit :: () -> WholeEdit (Maybe t) -> PureReadable SoupRead (Identity ((),[SoupEdit]));
        editLensPutEdit () (MkWholeEdit mt) = pure $ pure $ pure $ pure $ SoupEditSetPrimitive valkey mt;
    } in MkEditLens{..};

    soupTripleLens :: UUID -> UUID -> PureEditLens () SoupEdit (WholeEdit (Maybe UUID));
    soupTripleLens prd subj = let
    {
        editInitial = ();

        editGet :: () -> WholeReader (Maybe UUID) a -> PureReadable SoupRead a;
        editGet () ReadWhole = readable $ SoupReadGetValue prd subj;

        editUpdate :: SoupEdit -> () -> PureReadable SoupRead ((),[WholeEdit (Maybe UUID)]);
        editUpdate (SoupEditSetValue p s mv) () | p == prd && s == subj = pure $ pure $ pure $ MkWholeEdit mv;
        editUpdate _ () = pure $ pure [];

        editLensFunction = MkEditFunction{..};

        editLensPutEdit :: () -> WholeEdit (Maybe UUID) -> PureReadable SoupRead (Maybe ((),[SoupEdit]));
        editLensPutEdit () (MkWholeEdit mv) = pure $ pure $ pure $ pure $ SoupEditSetValue prd subj mv;
    } in MkEditLens{..};

    type SoupMorphism = PointedEditLens MonadIO Maybe SoupEdit;

    predicateSoupMorphism :: UUID -> SoupMorphism (WholeEdit (Maybe UUID)) (WholeEdit (Maybe UUID));
    predicateSoupMorphism prd = MkPointedEditLens $ let
    {
        editInitial = ();

        editGet :: forall t. () -> WholeReader (Maybe UUID) t -> IOReadable (PairEditReader SoupEdit (WholeEdit (Maybe UUID))) t;
        editGet () ReadWhole = do
        {
            msubj <- readable $ MkTupleEditReader EditSecond ReadWhole;
            case msubj of
            {
                Just subj -> readable $ MkTupleEditReader EditFirst $ SoupReadGetValue prd subj;
                Nothing -> return Nothing;
            };
        };

        editUpdate :: (PairEdit SoupEdit (WholeEdit (Maybe UUID))) -> () -> IOReadable (PairEditReader SoupEdit (WholeEdit (Maybe UUID))) ((),[WholeEdit (Maybe UUID)]);
        editUpdate (MkTupleEdit EditFirst (SoupEditSetValue p s mv)) () | p == prd = do
        {
            msubj <- readable $ MkTupleEditReader EditSecond ReadWhole;
            return $ pure $ if Just s == msubj then [MkWholeEdit mv] else [];
        };
        editUpdate (MkTupleEdit EditFirst (SoupEditDeleteLookupValue p v)) () | p == prd = do
        {
            msubj <- readable $ MkTupleEditReader EditSecond ReadWhole;
            case msubj of
            {
                Just subj -> do
                {
                    mval <- readable $ MkTupleEditReader EditFirst $ SoupReadGetValue p subj;
                    return $ pure $ if mval == Just v then [MkWholeEdit Nothing] else [];
                };
                Nothing -> return $ pure [];
            };
        };
        editUpdate (MkTupleEdit EditFirst (SoupEditDeleteTriple p s v)) () | p == prd = do
        {
            msubj <- readable $ MkTupleEditReader EditSecond ReadWhole;
            case msubj of
            {
                Just subj | subj == s -> do
                {
                    mval <- readable $ MkTupleEditReader EditFirst $ SoupReadGetValue p subj;
                    return $ pure $ if mval == Just v then [MkWholeEdit Nothing] else [];
                };
                _ -> return $ pure [];
            };
        };
        editUpdate (MkTupleEdit EditFirst _) () = return $ pure [];
        editUpdate (MkTupleEdit EditSecond (MkWholeEdit Nothing)) () = return $ pure [MkWholeEdit Nothing];
        editUpdate (MkTupleEdit EditSecond (MkWholeEdit (Just subj))) () = do
        {
            mval <- readable $ MkTupleEditReader EditFirst $ SoupReadGetValue prd subj;
            return $ pure $ [MkWholeEdit mval];
        };

        editLensFunction = MkEditFunction{..};

        editLensPutEdit :: () -> WholeEdit (Maybe UUID) -> IOReadable (PairEditReader SoupEdit (WholeEdit (Maybe UUID))) (Maybe ((),[PairEdit SoupEdit (WholeEdit (Maybe UUID))]));
        editLensPutEdit () (MkWholeEdit mv) = do
        {
            msubj <- readable $ MkTupleEditReader EditSecond ReadWhole;
            case msubj of
            {
                Just subj -> return $ pure $ pure [MkTupleEdit EditFirst $ SoupEditSetValue prd subj mv];
                Nothing -> do
                {
                    subj <- liftIO UUID.nextRandom;
                    return $ pure $ pure [MkTupleEdit EditSecond $ MkWholeEdit $ Just subj, MkTupleEdit EditFirst $ SoupEditSetValue prd subj mv];
                };
            };
        };
    } in MkEditLens{..};

    predicateInverseSoupMorphism :: UUID -> SoupMorphism (WholeEdit (Maybe UUID)) (FiniteSetEdit UUID);
    predicateInverseSoupMorphism prd = MkPointedEditLens $ let
    {
        editInitial = ();

        editGet :: forall t. () -> FiniteSetReader UUID t -> IOReadable (PairEditReader SoupEdit (WholeEdit (Maybe UUID))) t;
        editGet () KeyReadKeys = do
        {
            mval <- readable $ MkTupleEditReader EditSecond ReadWhole;
            case mval of
            {
                Just val -> readable $ MkTupleEditReader EditFirst $ SoupReadLookupValue prd val;
                Nothing -> return mempty;
            };
        };
        editGet () (KeyReadItem subj ReadWhole) = do
        {
            mval <- readable $ MkTupleEditReader EditSecond ReadWhole;
            case mval of
            {
                Just _ -> do
                {
                    val' <- readable $ MkTupleEditReader EditFirst $ SoupReadGetValue prd subj;
                    return $ if mval == val' then Just subj else Nothing;
                };
                Nothing -> return Nothing;
            };
        };

        editUpdate :: PairEdit SoupEdit (WholeEdit (Maybe UUID)) -> () -> IOReadable (PairEditReader SoupEdit (WholeEdit (Maybe UUID))) ((), [FiniteSetEdit UUID]);
        editUpdate (MkTupleEdit EditFirst (SoupEditSetValue p s mnewv)) () | p == prd = do
        {
            mval <- readable $ MkTupleEditReader EditSecond ReadWhole;
            case mval of
            {
                Just val -> do
                {
                    moldv <- readable $ MkTupleEditReader EditFirst $ SoupReadGetValue p s;
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
        editUpdate (MkTupleEdit EditFirst (SoupEditDeleteLookupValue p v)) () | p == prd = do
        {
            mval <- readable $ MkTupleEditReader EditSecond ReadWhole;
            return $ pure $ if mval == Just v then [KeyClear] else [];
        };
        editUpdate (MkTupleEdit EditFirst (SoupEditDeleteTriple p s v)) () | p == prd = do
        {
            mval <- readable $ MkTupleEditReader EditSecond ReadWhole;
            return $ pure $ if mval == Just v then [KeyDeleteItem s] else [];
        };
        editUpdate (MkTupleEdit EditFirst _) () = return $ pure [];
        editUpdate (MkTupleEdit EditSecond (MkWholeEdit Nothing)) () = return $ pure [KeyClear];
        editUpdate (MkTupleEdit EditSecond (MkWholeEdit (Just val))) () = do
        {
            subjs <- readable $ MkTupleEditReader EditFirst $ SoupReadLookupValue prd val;
            edits <- getReplaceEditsM @MonadIO subjs;
            return $ pure edits;
        };

        editLensFunction :: IOEditFunction () (PairEdit SoupEdit (WholeEdit (Maybe UUID))) (FiniteSetEdit UUID);
        editLensFunction = MkEditFunction{..};

        editLensPutEdit :: () -> FiniteSetEdit UUID -> IOReadable (PairEditReader SoupEdit (WholeEdit (Maybe UUID))) (Maybe ((), [PairEdit SoupEdit (WholeEdit (Maybe UUID))]));
        editLensPutEdit () (KeyEditItem _ edit) = never edit;
        editLensPutEdit () (KeyDeleteItem subj) = do
        {
            mval <- readable $ MkTupleEditReader EditSecond ReadWhole;
            case mval of
            {
                Just val -> return $ Just $ pure $ [MkTupleEdit EditFirst $ SoupEditDeleteTriple prd subj val];
                Nothing -> return $ Just $ pure [];
            };
        };
        editLensPutEdit () (KeyInsertReplaceItem subj) = do
        {
            mval <- readable $ MkTupleEditReader EditSecond ReadWhole;
            case mval of
            {
                Just val -> return $ Just $ pure [MkTupleEdit EditFirst $ SoupEditSetValue prd subj $ Just val];
                Nothing -> do
                {
                    val <- liftIO UUID.nextRandom;
                    return $ Just $ pure [MkTupleEdit EditSecond $ MkWholeEdit $ Just val, MkTupleEdit EditFirst $ SoupEditSetValue prd subj $ Just val];
                };
            };
        };
        editLensPutEdit () KeyClear = do
        {
            mval <- readable $ MkTupleEditReader EditSecond ReadWhole;
            case mval of
            {
                Just val -> return $ Just $ pure [MkTupleEdit EditFirst $ SoupEditDeleteLookupValue prd val];
                Nothing -> return $ Just $ pure [];
            };
        };
    } in MkEditLens{..};
}
