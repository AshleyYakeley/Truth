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

import Pinafore.Base
import Pinafore.Language
import Pinafore.Pinafore
import Pinafore.Storage
import Shapes
import System.FilePath
import Truth.Core

type FilePinaforeType = PinaforeAction TopType

filePinaforeType :: Text
filePinaforeType = qNegativeTypeDescription @PinaforeUpdate @FilePinaforeType

standardPinaforeContext :: FilePath -> UIToolkit -> CreateView (PinaforeContext PinaforeUpdate)
standardPinaforeContext dirpath uitoolkit = do
    rc <- viewGetResourceContext
    sqlReference <- liftIO $ sqlitePinaforeTableReference $ dirpath </> "tables.sqlite3"
    tableReference1 <- liftLifeCycleIO $ exclusiveResource rc sqlReference
    tableReference <- liftLifeCycleIO $ cacheReference rc 500000 tableReference1 -- half-second delay before writing
    memoryReference <- liftIO makeMemoryCellReference
    let
        picker :: forall update. PinaforeSelector update -> Premodel update ()
        picker PinaforeSelectPoint = reflectingPremodel $ pinaforeTableEntityReference $ tableReference rc
        picker PinaforeSelectFile = reflectingPremodel $ directoryPinaforeFileReference $ dirpath </> "files"
        picker PinaforeSelectMemory = reflectingPremodel memoryReference
    (sub, ()) <- liftLifeCycleIO $ makeSharedModel $ tuplePremodel picker
    liftLifeCycleIO $ makePinaforeContext sub uitoolkit

sqlitePinaforeDumpTable :: FilePath -> IO ()
sqlitePinaforeDumpTable dirpath = do
    MkAllF tables <- sqlitePinaforeTableGetEntireDatabase emptyResourceContext $ dirpath </> "tables.sqlite3"
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
       (?pinafore :: PinaforeContext PinaforeUpdate, FromPinaforeType PinaforeUpdate t)
    => FilePath
    -> Text
    -> InterpretResult t
pinaforeInterpretFileAtType puipath puitext = runPinaforeSourceScoped puipath $ parseValueAtType @PinaforeUpdate puitext

pinaforeInterpretFile :: (?pinafore :: PinaforeContext PinaforeUpdate) => FilePath -> Text -> InterpretResult (View ())
pinaforeInterpretFile puipath puitext = do
    action :: FilePinaforeType <- pinaforeInterpretFileAtType puipath puitext
    return $ runPinaforeAction $ fmap (\MkTopType -> ()) $ action

pinaforeInteractHandles :: (?pinafore :: PinaforeContext PinaforeUpdate) => Handle -> Handle -> Bool -> View ()
pinaforeInteractHandles inh outh echo = interact inh outh echo

pinaforeInteract :: (?pinafore :: PinaforeContext PinaforeUpdate) => View ()
pinaforeInteract = pinaforeInteractHandles stdin stdout False
