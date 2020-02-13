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
import Pinafore.Storage
import Shapes
import System.FilePath
import Truth.Core
import Truth.World.Clock

type FilePinaforeType = PinaforeAction ()

data PinaforeUpdate

filePinaforeType :: Text
filePinaforeType = qTypeDescription @PinaforeUpdate @FilePinaforeType

standardPinaforeContext :: FilePath -> UIToolkit -> LifeCycleIO PinaforeContext
standardPinaforeContext dirpath uitoolkit = do
    tableObject1 <- exclusiveResource $ sqlitePinaforeTableObject $ dirpath </> "tables.sqlite3"
    tableObject <- cacheObject 500000 tableObject1 -- half-second delay before writing
    memoryObject <- liftIO makeMemoryCellObject
    clockOM <- shareObjectMaker $ clockObjectMaker (UTCTime (fromGregorian 2000 1 1) 0) (secondsToNominalDiffTime 1)
    let
        picker :: forall update. PinaforeSelector update -> ObjectMaker update ()
        picker PinaforeSelectPoint = reflectingObjectMaker $ pinaforeTableEntityObject tableObject
        picker PinaforeSelectFile = reflectingObjectMaker $ directoryPinaforeFileObject $ dirpath </> "files"
        picker PinaforeSelectMemory = reflectingObjectMaker memoryObject
        picker PinaforeSelectClock = clockOM
        picker PinaforeSelectTimeZone = mapObjectMaker (liftReadOnlyFloatingEditLens clockTimeZoneLens) clockOM
    (rsub, ()) <- makeSharedSubscriber $ tupleObjectMaker picker
    (sub, uactions) <- liftIO $ undoQueueSubscriber rsub
    return $ MkPinaforeContext (unPinaforeAction uitoolkit uactions) sub

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
       (?pinafore :: PinaforeContext, FromPinaforeType PinaforeUpdate t) => FilePath -> Text -> InterpretResult t
pinaforeInterpretFileAtType puipath puitext = runPinaforeSourceScoped puipath $ parseValueAtType @PinaforeUpdate puitext

pinaforeInterpretFile :: (?pinafore :: PinaforeContext) => FilePath -> Text -> InterpretResult (IO ())
pinaforeInterpretFile puipath puitext = do
    action :: FilePinaforeType <- pinaforeInterpretFileAtType puipath puitext
    return $ runPinaforeAction action

pinaforeInteractHandles :: (?pinafore :: PinaforeContext) => Handle -> Handle -> Bool -> IO ()
pinaforeInteractHandles inh outh echo = interact inh outh echo

pinaforeInteract :: (?pinafore :: PinaforeContext) => IO ()
pinaforeInteract = pinaforeInteractHandles stdin stdout False
