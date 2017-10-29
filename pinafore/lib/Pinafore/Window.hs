module Pinafore.Window where
{
    import Shapes;
    import Truth.Core;
    import Pinafore.SQLite;
    import Pinafore.Example;

    sqlitePinaforeWindow :: FilePath -> IO (UIWindow ());
    sqlitePinaforeWindow sqlitepath = do
    {
        sub <- makeObjectSubscriber $ sqlitePinaforeObject sqlitepath;
        return $ MkUIWindow "Root" rootSpec sub;
    };
}
