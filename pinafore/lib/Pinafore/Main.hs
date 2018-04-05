module Pinafore.Main where

import Pinafore.Database.SQLite
import Pinafore.File
import Pinafore.Pinafore
import Pinafore.Query
import Shapes
import System.FilePath
import Truth.Core

type FilePinaforeType = QAction PinaforeEdit

filePinaforeType :: Text
filePinaforeType = qTypeDescription @FilePinaforeType

sqlitePinaforeObject :: FilePath -> Object PinaforeEdit
sqlitePinaforeObject dirpath =
    tupleObject $ \case
        PinaforeSelectTable -> sqlitePinaforeTableObject $ dirpath </> "tables.sqlite3"
        PinaforeSelectFile -> directoryPinaforeFileObject $ dirpath </> "files"

sqlitePinaforeMain :: FilePath -> (FilePath, Text) -> (UserInterface UIWindow () -> IO ()) -> IO ()
sqlitePinaforeMain dirpath (puipath, puitext) createWindow = do
    sub <- liftIO $ makeObjectSubscriber $ sqlitePinaforeObject dirpath
    let
        runView :: forall a. View PinaforeEdit a -> IO a
        runView view = let
            createView :: IO () -> CreateView PinaforeEdit (() -> LifeCycle a)
            createView _ = do
                a <- cvLiftView view
                return $ \() -> return a
            in subscribeView createView sub (\win -> createWindow $ MkUserInterface sub win) $ \_ -> Nothing
    MkComposeM action :: FilePinaforeType <-
        resultToM $ mapResultFailure unpack $ parseValue @PinaforeEdit puipath puitext
    result <- runView action
    case result of
        SuccessResult () -> return ()
        FailureResult msg -> fail $ unpack msg
