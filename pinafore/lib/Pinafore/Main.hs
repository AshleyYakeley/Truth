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

getSqlitePinaforeRunAction ::
       forall baseedit. Object baseedit -> (UserInterface UIWindow () -> IO ()) -> IO (UnliftIO (QActionM baseedit))
getSqlitePinaforeRunAction pinaforeObject createWindow = do
    sub <- liftIO $ makeObjectSubscriber pinaforeObject
    return $
        MkUnliftIO $ \(MkComposeM action :: QActionM baseedit a) -> do
            let
                createView :: IO () -> CreateView baseedit (() -> LifeCycle (Result Text a))
                createView _ = do
                    a <- cvLiftView action
                    return $ \() -> return a
            result <- subscribeView createView sub (\win -> createWindow $ MkUserInterface sub win) $ \_ -> Nothing
            case result of
                SuccessResult t -> return t
                FailureResult msg -> fail $ unpack msg

sqlitePinaforeMain :: FilePath -> (FilePath, Text) -> (UserInterface UIWindow () -> IO ()) -> IO ()
sqlitePinaforeMain dirpath (puipath, puitext) createWindow = do
    runAction <- getSqlitePinaforeRunAction (sqlitePinaforeObject dirpath) createWindow
    action :: FilePinaforeType <- resultTextToM $ parseValue @PinaforeEdit puipath puitext
    runUnliftIO runAction action

sqlitePinaforeInteractive :: FilePath -> (UserInterface UIWindow () -> IO ()) -> IO ()
sqlitePinaforeInteractive dirpath createWindow = do
    runAction <- getSqlitePinaforeRunAction (sqlitePinaforeObject dirpath) createWindow
    interact runAction
