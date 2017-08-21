{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE EmptyCase #-}
module Truth.World.Soup.Schema where
{
    import Truth.Core.Import;
    import Truth.Core;
    import Data.UUID;
    import Truth.World.SQLite(SQLData,FromField(..),ToField(..),fromSQLData);


    data SoupRead t where
    {
        SoupReadGetValue :: UUID -> UUID -> SoupRead (Maybe UUID);
        SoupReadLookupValue :: UUID -> UUID -> SoupRead [UUID];
        SoupReadLiteral :: (FromField t,ToField t) => UUID -> SoupRead (Maybe t);
        SoupReadLookupLiteral :: ToField t => UUID -> t -> SoupRead [UUID];
    };

    data SoupEdit where
    {
        SoupSetTriple :: UUID -> UUID -> UUID -> SoupEdit;
        SoupSetLiteral :: ToField t => UUID -> Maybe t -> SoupEdit;
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
}
