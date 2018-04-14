module Pinafore.Main
    ( filePinaforeType
    , PinaforeContext
    , sqliteWithPinaforeContext
    , pinaforeRunFile
    , pinaforeInteract
    ) where

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

sqlitePinaforeObject :: FilePath -> With (Object PinaforeEdit)
sqlitePinaforeObject dirpath cont =
    exclusiveObject (sqlitePinaforeTableObject $ dirpath </> "tables.sqlite3") $ \tableObject ->
        cont $
        tupleObject $ \case
            PinaforeSelectTable -> tableObject
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

newtype PinaforeContext =
    MkPinaforeContext (UnliftIO (QActionM PinaforeEdit))

sqliteWithPinaforeContext :: FilePath -> (UserInterface UIWindow () -> IO ()) -> With PinaforeContext
sqliteWithPinaforeContext dirpath createWindow cont =
    sqlitePinaforeObject dirpath $ \pinaforeObject -> do
        runAction <- getSqlitePinaforeRunAction pinaforeObject createWindow
        cont $ MkPinaforeContext runAction

pinaforeRunFile :: PinaforeContext -> FilePath -> Text -> IO ()
pinaforeRunFile (MkPinaforeContext runAction) puipath puitext = do
    action :: FilePinaforeType <- resultTextToM $ parseValue @PinaforeEdit puipath puitext
    runUnliftIO runAction action

pinaforeInteract :: PinaforeContext -> IO ()
pinaforeInteract (MkPinaforeContext runAction) = interact runAction
