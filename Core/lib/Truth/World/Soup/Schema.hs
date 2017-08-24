module Truth.World.Soup.Schema where
{
    import Truth.Core.Import;
    import Truth.Core;
    import Data.UUID;
    import Data.UUID.V4 as UUID;
    import Truth.World.SQLite(SQLData,FromField(..),ToField(..),fromSQLData,convertField);


    type SoupType t = (FromField t,ToField t);

    data SoupRead t where
    {
        SoupReadGetValue :: UUID -> UUID -> SoupRead (Maybe UUID);
        SoupReadLookupValue :: UUID -> UUID -> SoupRead [UUID];
        SoupReadLiteral :: SoupType t => UUID -> SoupRead (Maybe t);
        SoupReadLookupLiteral :: SoupType t => UUID -> t -> SoupRead [UUID];
    };

    data SoupEdit where
    {
        SoupSetTriple :: UUID -> UUID -> Maybe UUID -> SoupEdit;
        SoupSetLiteral :: SoupType t => UUID -> Maybe t -> SoupEdit;
    };

    instance Reader SoupRead where
    {
        type ReaderSubject SoupRead = ([(UUID,UUID,UUID)],[(UUID,SQLData)]);

        readFrom (triples,_) (SoupReadGetValue rp rs) = listToMaybe $ [v | (p,s,v) <- triples,p == rp && s == rs];
        readFrom (triples,_) (SoupReadLookupValue rp rv) = [s | (p,s,v) <- triples, p == rp, v == rv];
        readFrom (_,literals) (SoupReadLiteral rv) = do
        {
            d <- listToMaybe [l | (v,l) <- literals, v == rv];
            fromSQLData d;
        };
        readFrom (triples,literals) (SoupReadLookupLiteral rp rl) = [s | (p,s,v) <- triples, rp == p, (v',l) <- literals, v == v', l == toField rl];
    };

    instance Floating SoupEdit SoupEdit;
    instance Edit SoupEdit where
    {
        type EditReader SoupEdit = SoupRead;
        applyEdit _ _ = return undefined;
        invertEdit _ = return undefined;
    };

    soupLiteralLens :: forall t. SoupType t => UUID -> PureEditLens' Identity () SoupEdit (WholeEdit (Maybe t));
    soupLiteralLens valkey = let
    {
        editInitial = ();

        editGet :: () -> WholeReader (Maybe t) a -> PureReadable SoupRead a;
        editGet () ReadWhole = readable $ SoupReadLiteral valkey;

        editUpdate :: SoupEdit -> () -> PureReadable SoupRead ((),[WholeEdit (Maybe t)]);
        editUpdate (SoupSetLiteral k mt) () | k == valkey = pure $ pure $ pure $ MkWholeEdit $ mt >>= convertField;
        editUpdate _ () = pure $ pure [];

        editLensFunction = MkEditFunction{..};

        editLensPutEdit :: () -> WholeEdit (Maybe t) -> PureReadable SoupRead (Identity ((),[SoupEdit]));
        editLensPutEdit () (MkWholeEdit mt) = pure $ pure $ pure $ pure $ SoupSetLiteral valkey mt;
    } in MkEditLens{..};

    soupTripleLens :: UUID -> UUID -> PureEditLens () SoupEdit (WholeEdit (Maybe UUID));
    soupTripleLens prd subj = let
    {
        editInitial = ();

        editGet :: () -> WholeReader (Maybe UUID) a -> PureReadable SoupRead a;
        editGet () ReadWhole = readable $ SoupReadGetValue prd subj;

        editUpdate :: SoupEdit -> () -> PureReadable SoupRead ((),[WholeEdit (Maybe UUID)]);
        editUpdate (SoupSetTriple p s mv) () | p == prd && s == subj = pure $ pure $ pure $ MkWholeEdit mv;
        editUpdate _ () = pure $ pure [];

        editLensFunction = MkEditFunction{..};

        editLensPutEdit :: () -> WholeEdit (Maybe UUID) -> PureReadable SoupRead (Maybe ((),[SoupEdit]));
        editLensPutEdit () (MkWholeEdit mv) = pure $ pure $ pure $ pure $ SoupSetTriple prd subj mv;
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
        editUpdate (MkTupleEdit EditFirst (SoupSetTriple p s mv)) () | p == prd = do
        {
            msubj <- readable $ MkTupleEditReader EditSecond ReadWhole;
            return $ pure $ if Just s == msubj then [MkWholeEdit mv] else [];
        };
        editUpdate (MkTupleEdit EditFirst _) () = pure $ pure [];
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
                Just subj -> return $ pure $ pure [MkTupleEdit EditFirst $ SoupSetTriple prd subj mv];
                Nothing -> do
                {
                    subj <- liftIO UUID.nextRandom;
                    return $ pure $ pure [MkTupleEdit EditSecond $ MkWholeEdit $ Just subj, MkTupleEdit EditFirst $ SoupSetTriple prd subj mv];
                };
            };
        };
    } in MkEditLens{..};
}
