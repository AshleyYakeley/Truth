module Pinafore.Window where

import Pinafore.Database.SQLite
import Pinafore.File
import Pinafore.Pinafore
import Pinafore.Query
import Shapes
import System.FilePath
import Truth.Core

type FilePinaforeType = [UIWindow PinaforeEdit]

filePinaforeType :: Text
filePinaforeType = qTypeDescription @FilePinaforeType

sqlitePinaforeObject :: FilePath -> Object PinaforeEdit
sqlitePinaforeObject dirpath =
    tupleObject $ \case
        PinaforeSelectTable -> sqlitePinaforeTableObject $ dirpath </> "tables.sqlite3"
        PinaforeSelectFile -> directoryPinaforeFileObject $ dirpath </> "files"

sqlitePinaforeWindow :: FilePath -> (FilePath, Text) -> IO [UserInterface UIWindow ()]
sqlitePinaforeWindow dirpath (puipath, puitext) = do
    sub <- makeObjectSubscriber $ sqlitePinaforeObject dirpath
    windows :: FilePinaforeType <- resultToM $ mapResultFailure unpack $ parseValue @PinaforeEdit puipath puitext
    return $ fmap (MkUserInterface sub) windows
