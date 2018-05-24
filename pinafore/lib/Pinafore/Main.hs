module Pinafore.Main
    ( filePinaforeType
    , PinaforeContext
    , makePinaforeContext
    , sqlitePinaforeContext
    , sqlitePinaforeDumpTable
    , pinaforeRunFile
    , pinaforeInteract
    ) where

import Pinafore.Database
import Pinafore.Database.SQLite
import Pinafore.File
import Pinafore.Language
import Pinafore.Literal
import Pinafore.Pinafore
import Pinafore.Table
import Shapes
import System.FilePath
import Truth.Core

type FilePinaforeType = QAction PinaforeEdit

filePinaforeType :: Text
filePinaforeType = qTypeDescription @FilePinaforeType

sqlitePinaforeObject :: FilePath -> LifeCycle (Object PinaforeEdit)
sqlitePinaforeObject dirpath = do
    tableObject1 <- lifeCycleWith $ exclusiveObject $ sqlitePinaforeTableObject $ dirpath </> "tables.sqlite3"
    tableObject <- cacheObject 500000 tableObject1 -- half-second delay before writing
    return $
        tupleObject $ \case
            PinaforeSelectPoint -> pinaforeTablePointObject tableObject
            PinaforeSelectFile -> directoryPinaforeFileObject $ dirpath </> "files"

getPinaforeRunAction ::
       forall baseedit. Object baseedit -> (UserInterface UIWindow () -> IO ()) -> IO (UnliftIO (QActionM baseedit))
getPinaforeRunAction pinaforeObject createWindow = do
    sub <- liftIO $ makeObjectSubscriber pinaforeObject
    return $
        MkUnliftIO $ \(MkComposeM action :: QActionM baseedit a) -> do
            let
                createView :: IO () -> CreateView (ConstEdit Point) baseedit (() -> LifeCycle (Result Text a))
                createView _ = do
                    a <- cvLiftView action
                    return $ \() -> return a
            result <- subscribeView createView sub (\win -> createWindow $ MkUserInterface sub win) $ \_ -> Nothing
            case result of
                SuccessResult t -> return t
                FailureResult msg -> fail $ unpack msg

newtype PinaforeContext =
    MkPinaforeContext (UnliftIO (QActionM PinaforeEdit))

makePinaforeContext :: Object PinaforeEdit -> (UserInterface UIWindow () -> IO ()) -> IO PinaforeContext
makePinaforeContext pinaforeObject createWindow = do
    runAction <- liftIO $ getPinaforeRunAction pinaforeObject createWindow
    return $ MkPinaforeContext runAction

sqlitePinaforeContext :: FilePath -> (UserInterface UIWindow () -> IO ()) -> LifeCycle PinaforeContext
sqlitePinaforeContext dirpath createWindow = do
    pinaforeObject <- sqlitePinaforeObject dirpath
    liftIO $ makePinaforeContext pinaforeObject createWindow

sqlitePinaforeDumpTable :: FilePath -> IO ()
sqlitePinaforeDumpTable dirpath = do
    MkAllF tables <- sqlitePinaforeTableGetEntireDatabase $ dirpath </> "tables.sqlite3"
    for_ (tables $ MkTupleTableSel PinaforeTriple) $ \(MkAll row) -> let
        littable :: [(Point, Literal)]
        littable =
            fmap (\(MkAll lrow) -> (lrow LiteralKey, lrow LiteralValue)) $ tables $ MkTupleTableSel PinaforeLiteral
        p = row TriplePredicate
        s = row TripleSubject
        v = row TripleValue
        lv =
            case lookup v littable of
                Just l -> show l
                Nothing -> show v
        in putStrLn $ show p ++ " " ++ show s ++ " = " ++ lv

pinaforeRunFile :: PinaforeContext -> FilePath -> Text -> IO ()
pinaforeRunFile (MkPinaforeContext runAction) puipath puitext = do
    action :: FilePinaforeType <- resultTextToM $ parseValue @PinaforeEdit puipath puitext
    runUnliftIO runAction action

pinaforeInteract :: PinaforeContext -> IO ()
pinaforeInteract (MkPinaforeContext runAction) = interact runAction
