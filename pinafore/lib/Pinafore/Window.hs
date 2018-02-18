module Pinafore.Window where

import Pinafore.Edit
import Pinafore.Query
import Pinafore.SQLite
import Shapes
import Truth.Core
import Truth.Debug.Object

type FilePinaforeType = [UIWindow PinaforeEdit]

filePinaforeType :: Text
filePinaforeType = qTypeDescriptionFrom @FilePinaforeType

sqlitePinaforeWindow :: FilePath -> (FilePath, Text) -> IO [UserInterface UIWindow ()]
sqlitePinaforeWindow sqlitepath (puipath, puitext) = do
    sub <- makeObjectSubscriber $ traceArgThing "pinafore" $ sqlitePinaforeObject sqlitepath
    windows :: FilePinaforeType <- resultToM $ mapResultFailure unpack $ parseValue puipath puitext
    return $ fmap (MkUserInterface sub) windows
