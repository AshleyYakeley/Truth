module Pinafore.Window where
{
    import Shapes;
    import Truth.Core;
    import Pinafore.SQLite;
    import Pinafore.Query;

    sqlitePinaforeWindow :: FilePath -> (FilePath,String) -> IO (UIWindow ());
    sqlitePinaforeWindow sqlitepath (puipath,puitext) = do
    {
        sub <- makeObjectSubscriber $ sqlitePinaforeObject sqlitepath;
        spec <- resultToM $ parseValue puipath puitext;
        return $ MkUIWindow "Root" spec sub;
    };
}
