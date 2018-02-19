module Pinafore.Window where

import Pinafore.Database.SQLite
import Pinafore.Edit
import Pinafore.Query
import Shapes
import Truth.Core

type FilePinaforeType = [UIWindow PinaforeEdit]

filePinaforeType :: Text
filePinaforeType = qTypeDescriptionFrom @FilePinaforeType

sqlitePinaforeWindow :: FilePath -> (FilePath, Text) -> IO [UserInterface UIWindow ()]
sqlitePinaforeWindow sqlitepath (puipath, puitext) = do
    sub <- makeObjectSubscriber $ sqlitePinaforeObject sqlitepath
    windows :: FilePinaforeType <- resultToM $ mapResultFailure unpack $ parseValue puipath puitext
    return $ fmap (MkUserInterface sub) windows
