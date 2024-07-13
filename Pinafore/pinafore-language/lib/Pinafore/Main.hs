module Pinafore.Main
    ( ModuleOptions(..)
    , InvocationInfo(..)
    , standardLibraryContext
    , nullInvocationInfo
    , StorageModelOptions(..)
    , standardStorageModel
    , sqliteQDumpTable
    , qInterpretTextAtType
    , qInterpretText
    , qInterpretFile
    , qInteractHandles
    , qInteract
    ) where

import Import
import Pinafore.Context
import Pinafore.Language
import Pinafore.Language.Type
import Pinafore.Storage
import System.FilePath

data StorageModelOptions = MkStorageModelOptions
    { smoCache :: Bool
    , smoDataDir :: FilePath
    }

data ModuleOptions = MkModuleOptions
    { moExtraLibrary :: [LibraryModule ()]
    , moModuleDirs :: [FilePath]
    }

standardStorageModel :: StorageModelOptions -> View (Model QStorageUpdate)
standardStorageModel MkStorageModelOptions {..} = do
    rc <- viewGetResourceContext
    viewLiftLifecycle $ do
        sqlReference <- liftIO $ sqliteTableReference $ smoDataDir </> "tables.sqlite3"
        tableReference1 <- exclusiveResource rc sqlReference
        tableReference <-
            if smoCache
                then do
                    tableReferenceF <- cacheReference rc 500000 tableReference1 -- half-second delay before writing
                    return $ tableReferenceF rc
                else return tableReference1
        (model, ()) <- makeSharedModel $ reflectingPremodel $ qTableEntityReference tableReference
        return model

standardLoadModule :: ModuleOptions -> LoadModule
standardLoadModule MkModuleOptions {..} = let
    extraLibLoadModule :: LoadModule
    extraLibLoadModule = libraryLoadModule () moExtraLibrary
    dirLoadModule :: LoadModule
    dirLoadModule = mconcat $ fmap directoryLoadModule moModuleDirs
    in extraLibLoadModule <> dirLoadModule

standardLibraryContext :: InvocationInfo -> ModuleOptions -> LibraryContext
standardLibraryContext ii modopts = mkLibraryContext ii $ standardLoadModule modopts

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
       forall t m. (?library :: LibraryContext, HasQType QPolyShim 'Negative t, MonadIO m, MonadThrow QError m)
    => FilePath
    -> Text
    -> m t
qInterpretTextAtType puipath puitext = fromInterpretResult $ runPinaforeScoped puipath $ parseValueUnify puitext

qInterpretText :: (?library :: LibraryContext, MonadIO m, MonadThrow QError m) => FilePath -> Text -> m (View ())
qInterpretText puipath puitext = do
    action <- qInterpretTextAtType @(Action TopType) puipath puitext
    return $ runAction $ fmap (\MkTopType -> ()) $ action

qInterpretFile :: (?library :: LibraryContext) => FilePath -> View (View ())
qInterpretFile fpath = do
    ptext <- liftIO $ readFile fpath
    qInterpretText fpath $ decodeUtf8 $ toStrict ptext

qInteractHandles :: (?library :: LibraryContext) => Handle -> Handle -> Bool -> View ()
qInteractHandles inh outh echo = interact inh outh echo

qInteract :: (?library :: LibraryContext) => View ()
qInteract = qInteractHandles stdin stdout False
