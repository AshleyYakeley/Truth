module Pinafore.Window where

import Pinafore.Query
import Pinafore.SQLite
import Shapes
import Truth.Core

sqlitePinaforeWindow :: FilePath -> (FilePath, Text) -> IO [UIWindow ()]
sqlitePinaforeWindow sqlitepath (puipath, puitext) = do
    sub <- makeObjectSubscriber $ sqlitePinaforeObject sqlitepath
    windows <- resultToM $ mapResultFailure unpack $ parseValue puipath puitext
    return $ fmap (\(title :: Text, spec) -> MkUIWindow title spec sub) windows
