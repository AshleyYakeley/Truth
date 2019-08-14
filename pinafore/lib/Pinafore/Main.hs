module Pinafore.Main
    ( filePinaforeType
    , PinaforeContext
    , makePinaforeContext
    , standardPinaforeContext
    , sqlitePinaforeDumpTable
    , pinaforeInterpretFileAtType
    , pinaforeInterpretFile
    , pinaforeInteractHandles
    , pinaforeInteract
    ) where

import Data.Time
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
import Truth.World.Clock

type FilePinaforeType = PinaforeAction PinaforeEdit ()

filePinaforeType :: Text
filePinaforeType = qTypeDescription @PinaforeEdit @FilePinaforeType

standardPinaforeContext :: UpdateTiming -> FilePath -> UIToolkit -> LifeCycleIO (PinaforeContext PinaforeEdit)
standardPinaforeContext ut dirpath uitoolkit = do
    tableObject1 <- lifeCycleWith $ exclusiveObject $ sqlitePinaforeTableObject $ dirpath </> "tables.sqlite3"
    tableObject <- cacheObject 500000 tableObject1 -- half-second delay before writing
    memoryObject <- liftIO makeMemoryCellObject
    clockUO <-
        shareUpdatingObject False $
        clockUpdatingObject (UTCTime (fromGregorian 2000 1 1) 0) (secondsToNominalDiffTime 1)
    clockTimeEF <- liftIO makeClockTimeZoneEF
    let
        picker :: forall edit. PinaforeSelector edit -> UpdatingObject edit ()
        picker PinaforeSelectPoint = updatingObject $ pinaforeTableEntityObject tableObject
        picker PinaforeSelectFile = updatingObject $ directoryPinaforeFileObject $ dirpath </> "files"
        picker PinaforeSelectMemory = updatingObject memoryObject
        picker PinaforeSelectClock = clockUO
        picker PinaforeSelectTimeZone = lensUpdatingObject (readOnlyEditLens clockTimeEF) clockUO
    (sub, ()) <- makeSharedSubscriber ut $ tupleUpdatingObject picker
    makePinaforeContext sub uitoolkit

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

pinaforeInterpretFileAtType ::
       (?pinafore :: PinaforeContext PinaforeEdit, FromPinaforeType PinaforeEdit t)
    => FilePath
    -> Text
    -> InterpretResult t
pinaforeInterpretFileAtType puipath puitext = runPinaforeSourceScoped puipath $ parseValueAtType @PinaforeEdit puitext

pinaforeInterpretFile :: (?pinafore :: PinaforeContext PinaforeEdit) => FilePath -> Text -> InterpretResult (IO ())
pinaforeInterpretFile puipath puitext = do
    action :: FilePinaforeType <- pinaforeInterpretFileAtType puipath puitext
    return $ runPinaforeAction action

pinaforeInteractHandles :: (?pinafore :: PinaforeContext PinaforeEdit) => Handle -> Handle -> Bool -> IO ()
pinaforeInteractHandles inh outh echo = interact inh outh echo

pinaforeInteract :: (?pinafore :: PinaforeContext PinaforeEdit) => IO ()
pinaforeInteract = pinaforeInteractHandles stdin stdout False
