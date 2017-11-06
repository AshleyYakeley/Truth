module Pinafore.Window where

import Pinafore.Query
import Pinafore.SQLite
import Shapes
import Truth.Core

sqlitePinaforeWindow :: FilePath -> (FilePath, String) -> IO [UIWindow ()]
sqlitePinaforeWindow sqlitepath (puipath, puitext) = do
    sub <- makeObjectSubscriber $ sqlitePinaforeObject sqlitepath
    windows <- resultToM $ parseValue puipath puitext
    return $ fmap (\(title :: Text, spec) -> MkUIWindow (unpack title) spec sub) windows
