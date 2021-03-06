module Pinafore.Language.Library.FetchModule
    ( getLibraryModuleModule
    , FetchModule
    , runFetchModule
    , directoryFetchModule
    , libraryFetchModule
    , textFetchModule
    ) where

import Pinafore.Context
import Pinafore.Language.DefDoc
import Pinafore.Language.DocTree
import Pinafore.Language.Error
import Pinafore.Language.Grammar
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Defs
import Pinafore.Language.Name
import Pinafore.Language.Type
import Shapes
import System.Directory (doesFileExist)
import System.FilePath

newtype FetchModule = MkFetchModule
    { runFetchModule :: (?pinafore :: PinaforeContext) =>
                                PinaforeScope -> ModuleName -> PinaforeInterpreter (Maybe PinaforeModule)
    }

instance Semigroup FetchModule where
    MkFetchModule fma <> MkFetchModule fmb =
        MkFetchModule $ \implictScope mname -> do
            mrr <- fma implictScope mname
            case mrr of
                Just rr -> return $ Just rr
                Nothing -> fmb implictScope mname

instance Monoid FetchModule where
    mempty = MkFetchModule $ \_ _ -> return Nothing

loadModuleFromText :: PinaforeScope -> ModuleName -> FilePath -> Text -> PinaforeInterpreter PinaforeModule
loadModuleFromText implictScope modname fpath text =
    importScope implictScope $ runSourcePos (initialPos fpath) $ parseModule modname text

loadModuleFromByteString ::
       PinaforeScope -> ModuleName -> FilePath -> LazyByteString -> PinaforeInterpreter PinaforeModule
loadModuleFromByteString implictScope modname fpath bs =
    case eitherToResult $ decodeUtf8' $ toStrict bs of
        SuccessResult text -> loadModuleFromText implictScope modname fpath text
        FailureResult err -> throwErrorType (initialPos fpath) $ UnicodeDecodeError $ pack $ show err

textFetchModule :: (ModuleName -> IO (Maybe Text)) -> FetchModule
textFetchModule getText =
    MkFetchModule $ \implictScope modname -> do
        mtext <- liftIO $ getText modname
        for mtext $ \text -> loadModuleFromText implictScope modname (show modname) text

moduleRelativePath :: ModuleName -> FilePath
moduleRelativePath (MkModuleName nn) = (foldl1 (</>) $ fmap unpack nn) <> ".pinafore"

directoryFetchModule :: FilePath -> FetchModule
directoryFetchModule dirpath =
    MkFetchModule $ \implictScope modname -> do
        let fpath = dirpath </> moduleRelativePath modname
        found <- liftIO $ doesFileExist fpath
        case found of
            False -> return Nothing
            True -> do
                bs <- liftIO $ readFile fpath
                mm <- loadModuleFromByteString implictScope modname fpath bs
                return $ Just mm

getLibraryModuleModule :: (?pinafore :: PinaforeContext) => LibraryModule -> IO PinaforeModule
getLibraryModuleModule libmod = do
    let
        bindDocs :: [BindDoc]
        bindDocs = toList libmod
        seBinding :: ScopeEntry -> Maybe (Name, PinaforeBinding)
        seBinding (BindScopeEntry name mb) = do
            b <- mb
            return (name, b ?pinafore)
        seBinding _ = Nothing
        getEntry :: BindDoc -> Maybe (Name, DocInterpreterBinding PinaforeTypeSystem)
        getEntry MkBindDoc {..} = do
            (name, b) <- seBinding bdScopeEntry
            return (name, (docDescription bdDoc, b))
        bscope :: PinaforeScope
        bscope = bindingsScope $ mapFromList $ mapMaybe getEntry bindDocs
    dscopes <-
        for bindDocs $ \bd ->
            case bdScopeEntry bd of
                BindScopeEntry _ _ -> return mempty
                SubtypeScopeEntry entries -> getSubtypesScope entries
    let
        moduleDoc = fmap bdDoc libmod
        moduleScope = bscope <> mconcat dscopes
    return $ MkModule {..}

libraryFetchModule :: [LibraryModule] -> FetchModule
libraryFetchModule lmods = let
    m :: Map Text LibraryModule
    m = mapFromList $ fmap (\lmod -> (docTreeName lmod, lmod)) lmods
    in MkFetchModule $ \_ mname -> liftIO $ for (lookup (toText mname) m) getLibraryModuleModule
