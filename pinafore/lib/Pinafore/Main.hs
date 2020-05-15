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
import Pinafore.Storage
import Shapes
import System.FilePath
import Truth.Core

type FilePinaforeType = PinaforeAction TopType

filePinaforeType :: Text
filePinaforeType = qNegativeTypeDescription @FilePinaforeType

standardPinaforeContext :: FilePath -> UIToolkit -> CreateView PinaforeContext
standardPinaforeContext dirpath uitoolkit = do
    rc <- viewGetResourceContext
    sqlReference <- liftIO $ sqlitePinaforeTableReference $ dirpath </> "tables.sqlite3"
    tableReference1 <- liftLifeCycleIO $ exclusiveResource rc sqlReference
    tableReference <- liftLifeCycleIO $ cacheReference rc 500000 tableReference1 -- half-second delay before writing
    (model, ()) <-
        liftLifeCycleIO $ makeSharedModel $ reflectingPremodel $ pinaforeTableEntityReference $ tableReference rc
    liftLifeCycleIO $ makePinaforeContext model uitoolkit

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
       (?pinafore :: PinaforeContext, FromPinaforeType t) => FilePath -> Text -> InterpretResult t
pinaforeInterpretFileAtType puipath puitext = runPinaforeSourceScoped puipath $ parseValueAtType puitext

pinaforeInterpretFile :: (?pinafore :: PinaforeContext) => FilePath -> Text -> InterpretResult (View ())
pinaforeInterpretFile puipath puitext = do
    action :: FilePinaforeType <- pinaforeInterpretFileAtType puipath puitext
    return $ runPinaforeAction $ fmap (\MkTopType -> ()) $ action

pinaforeInteractHandles :: (?pinafore :: PinaforeContext) => Handle -> Handle -> Bool -> View ()
pinaforeInteractHandles inh outh echo = interact inh outh echo

pinaforeInteract :: (?pinafore :: PinaforeContext) => View ()
pinaforeInteract = pinaforeInteractHandles stdin stdout False
