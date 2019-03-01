module Pinafore.Main
    ( filePinaforeType
    , PinaforeContext
    , makePinaforeContext
    , sqlitePinaforeContext
    , sqlitePinaforeDumpTable
    , pinaforeInterpretFile
    , pinaforeInteractHandles
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
import Truth.UI.GTK

type FilePinaforeType = PinaforeAction PinaforeEdit ()

filePinaforeType :: Text
filePinaforeType = qTypeDescription @PinaforeEdit @FilePinaforeType

sqlitePinaforeObject :: FilePath -> LifeCycle (Object PinaforeEdit)
sqlitePinaforeObject dirpath = do
    tableObject1 <- lifeCycleWith $ exclusiveObject $ sqlitePinaforeTableObject $ dirpath </> "tables.sqlite3"
    tableObject <- cacheObject 500000 tableObject1 -- half-second delay before writing
    return $
        tupleObject $ \case
            PinaforeSelectPoint -> pinaforeTableEntityObject tableObject
            PinaforeSelectFile -> directoryPinaforeFileObject $ dirpath </> "files"

sqlitePinaforeContext ::
       FilePath
    -> (forall actions. UserInterface WindowSpec actions -> IO (UIWindow, actions))
    -> LifeCycle (PinaforeContext PinaforeEdit)
sqlitePinaforeContext dirpath createWindow = do
    pinaforeObject <- sqlitePinaforeObject dirpath
    makePinaforeContext pinaforeObject createWindow

sqlitePinaforeDumpTable :: FilePath -> IO ()
sqlitePinaforeDumpTable dirpath = do
    MkAllF tables <- sqlitePinaforeTableGetEntireDatabase $ dirpath </> "tables.sqlite3"
    let
        littable :: [(Entity, Literal)]
        littable =
            fmap (\(MkAllValue lrow) -> (lrow LiteralKey, lrow LiteralValue)) $ tables $ MkTupleTableSel PinaforeLiteral
    for_ (tables $ MkTupleTableSel PinaforeTriple) $ \(MkAllValue row) -> let
        p = row TriplePredicate
        s = row TripleSubject
        v = row TripleValue
        lv =
            case lookup v littable of
                Just l -> show l
                Nothing -> show v
        in putStrLn $ show p ++ " " ++ show s ++ " = " ++ lv

pinaforeInterpretFile :: (?pinafore :: PinaforeContext PinaforeEdit) => FilePath -> Text -> IO (IO ())
pinaforeInterpretFile puipath puitext = do
    action :: FilePinaforeType <-
        resultTextToM $ runPinaforeSourceScoped puipath $ parseValueAtType @PinaforeEdit puitext
    return $ runPinaforeAction action

pinaforeInteractHandles :: (?pinafore :: PinaforeContext PinaforeEdit) => Handle -> Handle -> Bool -> IO ()
pinaforeInteractHandles inh outh echo = interact inh outh echo

pinaforeInteract :: (?pinafore :: PinaforeContext PinaforeEdit) => IO ()
pinaforeInteract = pinaforeInteractHandles stdin stdout False
