module Pinafore.Window where
{
    import Shapes;
    import Truth.Core;
    import Pinafore.SQLite;
    import Pinafore.Query;

    sqlitePinaforeWindow :: FilePath -> (FilePath,String) -> IO [UIWindow ()];
    sqlitePinaforeWindow sqlitepath (puipath,puitext) = do
    {
        sub <- makeObjectSubscriber $ sqlitePinaforeObject sqlitepath;
        windows <- resultToM $ parseValue puipath puitext;
        return $ fmap (\(title :: Text,spec) -> MkUIWindow (unpack title) spec sub) windows;
    };
}
