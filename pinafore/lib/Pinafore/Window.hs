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
filePinaforeType = qTypeDescriptionFrom @PinaforeEdit @FilePinaforeType

sqlitePinaforeWindow :: FilePath -> (FilePath, Text) -> IO [UserInterface UIWindow ()]
sqlitePinaforeWindow dirpath (puipath, puitext) = do
    sub <-
        makeSharedSubscriber $ objectSubscriber $
        asyncPushObject $
        protectObject $
        tupleObject $ \case
            PinaforeSelectTable -> sqlitePinaforeTableObject $ dirpath </> "tables.sqlite3"
            PinaforeSelectFile -> directoryPinaforeFileObject $ dirpath </> "files"
    windows :: FilePinaforeType <- resultToM $ mapResultFailure unpack $ parseValue @PinaforeEdit puipath puitext
    return $ fmap (MkUserInterface sub) windows
