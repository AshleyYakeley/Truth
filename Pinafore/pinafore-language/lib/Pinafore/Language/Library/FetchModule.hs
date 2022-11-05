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
    { runFetchModule :: (?qcontext :: QContext) => ModuleName -> QInterpreter (Maybe QModule)
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

loadModuleFromText :: ModuleName -> Text -> QInterpreter QModule
loadModuleFromText modname text =
    transformTMap (void $ interpretImportDeclaration builtInModuleName) $ parseModule modname text

loadModuleFromByteString :: ModuleName -> LazyByteString -> QInterpreter QModule
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

getLibraryModuleModule :: (?qcontext :: QContext) => LibraryModule -> QInterpreter QModule
getLibraryModuleModule libmod = do
    let
        bindDocs :: [BindDoc]
        bindDocs = toList libmod
        seBinding :: ScopeEntry -> Maybe (FullName, QInterpreterBinding)
        seBinding (BindScopeEntry name mb) = do
            b <- mb
            return (name, b ?qcontext)
        seBinding _ = Nothing
        getEntry :: BindDoc -> Maybe QBindingInfo
        getEntry MkBindDoc {..} = do
            (biName, biValue) <- seBinding bdScopeEntry
            let biDocumentation = docDescription bdDoc
            return MkBindingInfo {..}
        bscope :: QScope
        bscope = bindingInfosToScope $ mapMaybe getEntry bindDocs
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
