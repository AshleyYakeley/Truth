module Pinafore.Main
    ( ModuleOptions(..)
    , standardFetchModule
    , QContext
    , nullQContext
    , makeQContext
    , ContextOptions(..)
    , standardQContext
    , sqliteQDumpTable
    , qInterpretTextAtType
    , qInterpretText
    , qInterpretFile
    , qInteractHandles
    , qInteract
    ) where

import Changes.Core
import Data.Shim
import Pinafore.Base
import Pinafore.Context
import Pinafore.Language
import Pinafore.Storage
import Shapes
import System.FilePath

data ModuleOptions = MkModuleOptions
    { moExtraLibrary :: [LibraryModule]
    , moModuleDirs :: [FilePath]
    }

data ContextOptions = MkContextOptions
    { coCache :: Bool
    , coDataDir :: FilePath
    }

standardStorageModel :: Bool -> FilePath -> View (Model QStorageUpdate)
standardStorageModel cache dataDir = do
    rc <- viewGetResourceContext
    viewLiftLifecycle $ do
        sqlReference <- liftIO $ sqliteTableReference $ dataDir </> "tables.sqlite3"
        tableReference1 <- exclusiveResource rc sqlReference
        tableReference <-
            if cache
                then do
                    tableReferenceF <- cacheReference rc 500000 tableReference1 -- half-second delay before writing
                    return $ tableReferenceF rc
                else return tableReference1
        (model, ()) <- makeSharedModel $ reflectingPremodel $ qTableEntityReference tableReference
        return model

standardFetchModule :: ModuleOptions -> FetchModule
standardFetchModule MkModuleOptions {..} = let
    extraLibFetchModule :: FetchModule
    extraLibFetchModule = libraryFetchModule moExtraLibrary
    dirFetchModule :: FetchModule
    dirFetchModule = mconcat $ fmap directoryFetchModule moModuleDirs
    in extraLibFetchModule <> dirFetchModule

standardQContext :: ContextOptions -> InvocationInfo -> View QContext
standardQContext MkContextOptions {..} invinfo = do
    model <- standardStorageModel coCache coDataDir
    pc <- viewLiftLifecycle $ makeQContext invinfo model
    return pc

sqliteQDumpTable :: FilePath -> IO ()
sqliteQDumpTable dirpath = do
    MkAllFor tables <- sqliteTableGetEntireDatabase emptyResourceContext $ dirpath </> "tables.sqlite3"
    let
        littable :: [(Entity, Literal)]
        littable = fmap (\(MkAllOf lrow) -> (lrow LiteralKey, lrow LiteralValue)) $ tables $ MkTupleTableSel QSLiteral
    for_ (tables $ MkTupleTableSel QSProperty) $ \(MkAllOf row) -> let
        p = row TriplePredicate
        s = row TripleSubject
        v = row TripleValue
        lv =
            case lookup v littable of
                Just l -> show l
                Nothing -> show v
        in putStrLn $ show p ++ " " ++ show s ++ " = " ++ lv

qInterpretTextAtType ::
       forall t. (?qcontext :: QContext, ?library :: LibraryContext, HasQType 'Negative t)
    => FilePath
    -> Text
    -> InterpretResult t
qInterpretTextAtType puipath puitext = runPinaforeScoped puipath $ parseValueUnify puitext

qInterpretText :: (?qcontext :: QContext, ?library :: LibraryContext) => FilePath -> Text -> InterpretResult (View ())
qInterpretText puipath puitext = do
    action <- qInterpretTextAtType @(Action TopType) puipath puitext
    return $ runAction $ fmap (\MkTopType -> ()) $ action

qInterpretFile ::
       (?qcontext :: QContext, ?library :: LibraryContext, MonadIO m, MonadThrow PinaforeError m)
    => FilePath
    -> m (View ())
qInterpretFile fpath = do
    ptext <- liftIO $ readFile fpath
    fromInterpretResult $ qInterpretText fpath $ decodeUtf8 $ toStrict ptext

qInteractHandles :: (?qcontext :: QContext, ?library :: LibraryContext) => Handle -> Handle -> Bool -> View ()
qInteractHandles inh outh echo = interact inh outh echo

qInteract :: (?qcontext :: QContext, ?library :: LibraryContext) => View ()
qInteract = qInteractHandles stdin stdout False
