module Pinafore.Main
    ( filePinaforeType
    , PinaforeContext
    , makePinaforeContext
    , standardPinaforeContext
    , sqlitePinaforeDumpTable
    , pinaforeInterpretTextAtType
    , pinaforeInterpretText
    , pinaforeInterpretFile
    , pinaforeInteractHandles
    , pinaforeInteract
    ) where

import Changes.Core
import Pinafore.Base
import Pinafore.Context
import Pinafore.Language
import Pinafore.Storage
import Shapes
import System.FilePath

type FilePinaforeType = PinaforeAction TopType

filePinaforeType :: Text
filePinaforeType = qNegativeTypeDescription @FilePinaforeType

doCache :: Bool
doCache = True

standardPinaforeContext :: InvocationInfo -> FilePath -> ChangesContext -> CreateView PinaforeContext
standardPinaforeContext invinfo dirpath tc = do
    rc <- viewGetResourceContext
    liftLifeCycleIO $ do
        sqlReference <- liftIO $ sqlitePinaforeTableReference $ dirpath </> "tables.sqlite3"
        tableReference1 <- exclusiveResource rc sqlReference
        tableReference <-
            if doCache
                then do
                    tableReferenceF <- cacheReference rc 500000 tableReference1 -- half-second delay before writing
                    return $ tableReferenceF rc
                else return tableReference1
        (model, ()) <- makeSharedModel $ reflectingPremodel $ pinaforeTableEntityReference tableReference
        makePinaforeContext invinfo model tc

sqlitePinaforeDumpTable :: FilePath -> IO ()
sqlitePinaforeDumpTable dirpath = do
    MkAllF tables <- sqlitePinaforeTableGetEntireDatabase emptyResourceContext $ dirpath </> "tables.sqlite3"
    let
        littable :: [(Entity, Literal)]
        littable =
            fmap (\(MkAllValue lrow) -> (lrow LiteralKey, lrow LiteralValue)) $ tables $ MkTupleTableSel PinaforeLiteral
    for_ (tables $ MkTupleTableSel PinaforeProperty) $ \(MkAllValue row) -> let
        p = row TriplePredicate
        s = row TripleSubject
        v = row TripleValue
        lv =
            case lookup v littable of
                Just l -> show l
                Nothing -> show v
        in putStrLn $ show p ++ " " ++ show s ++ " = " ++ lv

pinaforeInterpretTextAtType ::
       (?pinafore :: PinaforeContext, FromPinaforeType t) => FilePath -> Text -> InterpretResult t
pinaforeInterpretTextAtType puipath puitext = runPinaforeSourceScoped puipath $ parseValueUnify puitext

pinaforeInterpretText :: (?pinafore :: PinaforeContext) => FilePath -> Text -> InterpretResult (View ())
pinaforeInterpretText puipath puitext = do
    action :: FilePinaforeType <- pinaforeInterpretTextAtType puipath puitext
    return $ runPinaforeAction $ fmap (\MkTopType -> ()) $ action

pinaforeInterpretFile ::
       (?pinafore :: PinaforeContext, MonadIO m, MonadThrow PinaforeError m) => FilePath -> m (View ())
pinaforeInterpretFile fpath = do
    ptext <- liftIO $ readFile fpath
    throwResult $ pinaforeInterpretText fpath $ decodeUtf8 $ toStrict ptext

pinaforeInteractHandles :: (?pinafore :: PinaforeContext) => Handle -> Handle -> Bool -> View ()
pinaforeInteractHandles inh outh echo = interact inh outh echo

pinaforeInteract :: (?pinafore :: PinaforeContext) => View ()
pinaforeInteract = pinaforeInteractHandles stdin stdout False
