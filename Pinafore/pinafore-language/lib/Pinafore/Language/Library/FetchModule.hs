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
    { runFetchModule :: (?pinafore :: PinaforeContext) => ModuleName -> PinaforeInterpreter (Maybe PinaforeModule)
    }

instance Semigroup FetchModule where
    MkFetchModule fma <> MkFetchModule fmb =
        MkFetchModule $ \mname -> do
            mrr <- fma mname
            case mrr of
                Just rr -> return $ Just rr
                Nothing -> fmb mname

instance Monoid FetchModule where
    mempty = MkFetchModule $ \_ -> return Nothing

loadModuleFromText :: ModuleName -> Text -> PinaforeInterpreter PinaforeModule
loadModuleFromText modname text =
    unmapTransformT (void $ interpretImportDeclaration stdModuleName) $ parseModule modname text

loadModuleFromByteString :: ModuleName -> LazyByteString -> PinaforeInterpreter PinaforeModule
loadModuleFromByteString modname bs =
    case eitherToResult $ decodeUtf8' $ toStrict bs of
        SuccessResult text -> loadModuleFromText modname text
        FailureResult err -> throw $ UnicodeDecodeError $ pack $ show err

textFetchModule :: (ModuleName -> IO (Maybe Text)) -> FetchModule
textFetchModule getText =
    MkFetchModule $ \modname -> do
        mtext <- liftIO $ getText modname
        for mtext $ \text -> paramWith sourcePosParam (initialPos $ show modname) $ loadModuleFromText modname text

moduleRelativePath :: ModuleName -> FilePath
moduleRelativePath (MkModuleName nn) = (foldl1 (</>) $ fmap unpack nn) <> ".pinafore"

directoryFetchModule :: FilePath -> FetchModule
directoryFetchModule dirpath =
    MkFetchModule $ \modname -> do
        let fpath = dirpath </> moduleRelativePath modname
        found <- liftIO $ doesFileExist fpath
        case found of
            False -> return Nothing
            True -> do
                bs <- liftIO $ readFile fpath
                mm <- paramWith sourcePosParam (initialPos fpath) $ loadModuleFromByteString modname bs
                return $ Just mm

getLibraryModuleModule :: (?pinafore :: PinaforeContext) => LibraryModule -> PinaforeInterpreter PinaforeModule
getLibraryModuleModule libmod = do
    let
        bindDocs :: [BindDoc]
        bindDocs = toList libmod
        seBinding :: ScopeEntry -> Maybe (Name, PinaforeInterpreterBinding)
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
                BindScopeEntry _ _ -> return emptyScope
                SubtypeScopeEntry entry -> getSubtypeScope entry
    let moduleDoc = fmap bdDoc libmod
    moduleScope <- joinAllScopes dscopes bscope
    return $ MkModule {..}

libraryFetchModule :: [LibraryModule] -> FetchModule
libraryFetchModule lmods = let
    m :: Map Text LibraryModule
    m = mapFromList $ fmap (\lmod -> (docTreeName lmod, lmod)) lmods
    in MkFetchModule $ \mname -> for (lookup (toText mname) m) getLibraryModuleModule
