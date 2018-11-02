module Pinafore.Main
    ( filePinaforeType
    , PinaforeContext
    , makePinaforeContext
    , sqlitePinaforeContext
    , sqlitePinaforeDumpTable
    , pinaforeRunFile
    , pinaforeInteract
    ) where

import Pinafore.Base
import Pinafore.Language
import Pinafore.Pinafore
import Pinafore.Storage.Database
import Pinafore.Storage.Database.SQLite
import Pinafore.Storage.File
import Pinafore.Storage.Table
import Shapes
import System.FilePath
import Truth.Core

type FilePinaforeType = PinaforeAction PinaforeEdit

filePinaforeType :: Text
filePinaforeType = qTypeDescription @PinaforeEdit @FilePinaforeType

sqlitePinaforeObject :: FilePath -> LifeCycle (Object PinaforeEdit)
sqlitePinaforeObject dirpath = do
    tableObject1 <- lifeCycleWith $ exclusiveObject $ sqlitePinaforeTableObject $ dirpath </> "tables.sqlite3"
    tableObject <- cacheObject 500000 tableObject1 -- half-second delay before writing
    return $
        tupleObject $ \case
            PinaforeSelectPoint -> pinaforeTablePointObject tableObject
            PinaforeSelectFile -> directoryPinaforeFileObject $ dirpath </> "files"

getPinaforeRunAction ::
       forall baseedit.
       Object baseedit
    -> (UserInterface UIWindow () -> IO ())
    -> IO (UnliftIO (PinaforeActionM baseedit))
getPinaforeRunAction pinaforeObject createWindow = do
    sub <- liftIO $ makeObjectSubscriber pinaforeObject
    return $
        MkUnliftIO $ \(MkComposeM action :: PinaforeActionM baseedit a) -> do
            let
                createView :: IO () -> CreateView (ConstEdit Entity) baseedit (() -> LifeCycle (Result Text a))
                createView _ = do
                    a <- cvLiftView action
                    return $ \() -> return a
            result <- subscribeView createView sub (\win -> createWindow $ MkUserInterface sub win) $ \_ -> Nothing
            case result of
                SuccessResult t -> return t
                FailureResult msg -> fail $ unpack msg

newtype PinaforeContext =
    MkPinaforeContext (UnliftIO (PinaforeActionM PinaforeEdit))

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
    for_ (tables $ MkTupleTableSel PinaforeTriple) $ \(MkAllValue row) -> let
        littable :: [(Point, Literal)]
        littable =
            fmap (\(MkAllValue lrow) -> (lrow LiteralKey, lrow LiteralValue)) $ tables $ MkTupleTableSel PinaforeLiteral
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
    action :: FilePinaforeType <- resultTextToM $ parseValueAtType @PinaforeEdit puipath puitext
    runUnliftIO runAction action

pinaforeInteract :: PinaforeContext -> IO ()
pinaforeInteract (MkPinaforeContext runAction) = interact runAction
