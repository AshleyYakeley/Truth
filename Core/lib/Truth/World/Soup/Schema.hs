module Truth.World.Soup.Schema where
{
    import Truth.Core.Import;
    import Truth.Core;
    import Data.UUID;
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
        SoupSetTriple :: UUID -> UUID -> UUID -> SoupEdit;
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
        editUpdate _ () = return ((),[]);

        editLensFunction = MkEditFunction{..};

        editLensPutEdit :: () -> WholeEdit (Maybe t) -> PureReadable SoupRead (Identity ((),[SoupEdit]));
        editLensPutEdit () (MkWholeEdit mt) = pure $ pure $ pure $ pure $ SoupSetLiteral valkey mt;
    } in MkEditLens{..};
}
