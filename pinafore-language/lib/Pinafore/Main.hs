module Pinafore.Main
    ( PinaforeContext
    , makePinaforeContext
    , ContextOptions(..)
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

data ContextOptions = MkContextOptions
    { coCache :: Bool
    , coExtraLibrary :: [LibraryModule]
    , coModuleDirs :: [FilePath]
    , coDataDir :: FilePath
    }

standardStorageModel :: Bool -> FilePath -> CreateView (Model PinaforeStorageUpdate)
standardStorageModel cache dataDir = do
    rc <- viewGetResourceContext
    liftLifeCycle $ do
        sqlReference <- liftIO $ sqlitePinaforeTableReference $ dataDir </> "tables.sqlite3"
        tableReference1 <- exclusiveResource rc sqlReference
        tableReference <-
            if cache
                then do
                    tableReferenceF <- cacheReference rc 500000 tableReference1 -- half-second delay before writing
                    return $ tableReferenceF rc
                else return tableReference1
        (model, ()) <- makeSharedModel $ reflectingPremodel $ pinaforeTableEntityReference tableReference
        return model

standardFileModel :: FilePath -> CreateView (Model PinaforeFileUpdate)
standardFileModel fdir = do
    (model, ()) <- liftLifeCycle $ makeSharedModel $ reflectingPremodel $ directoryPinaforeFileReference fdir
    return model

standardPinaforeContext ::
       ContextOptions -> InvocationInfo -> ChangesContext -> CreateView (PinaforeContext, FetchModule)
standardPinaforeContext MkContextOptions {..} invinfo cc = do
    let
        extraLibFetchModule :: FetchModule
        extraLibFetchModule = libraryFetchModule coExtraLibrary
        dirFetchModule :: FetchModule
        dirFetchModule = mconcat $ fmap directoryFetchModule coModuleDirs
    storageModel <- standardStorageModel coCache coDataDir
    fileModel <- standardFileModel $ coDataDir </> "files"
    pc <- liftLifeCycle $ makePinaforeContext invinfo stdout storageModel fileModel cc
    return (pc, extraLibFetchModule <> dirFetchModule)

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
       (?pinafore :: PinaforeContext, ?library :: LibraryContext, FromPinaforeType t)
    => FilePath
    -> Text
    -> InterpretResult t
pinaforeInterpretTextAtType puipath puitext = runPinaforeSourceScoped puipath $ parseValueUnify puitext

pinaforeInterpretText ::
       (?pinafore :: PinaforeContext, ?library :: LibraryContext) => FilePath -> Text -> InterpretResult (View ())
pinaforeInterpretText puipath puitext = do
    action :: FilePinaforeType <- pinaforeInterpretTextAtType puipath puitext
    return $ runPinaforeAction $ fmap (\MkTopType -> ()) $ action

pinaforeInterpretFile ::
       (?pinafore :: PinaforeContext, ?library :: LibraryContext, MonadIO m, MonadThrow PinaforeError m)
    => FilePath
    -> m (View ())
pinaforeInterpretFile fpath = do
    ptext <- liftIO $ readFile fpath
    throwInterpretResult $ pinaforeInterpretText fpath $ decodeUtf8 $ toStrict ptext

pinaforeInteractHandles ::
       (?pinafore :: PinaforeContext, ?library :: LibraryContext) => Handle -> Handle -> Bool -> View ()
pinaforeInteractHandles inh outh echo = interact inh outh echo

pinaforeInteract :: (?pinafore :: PinaforeContext, ?library :: LibraryContext) => View ()
pinaforeInteract = pinaforeInteractHandles stdin stdout False
