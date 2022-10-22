module Pinafore.Language.Library.FetchModule
    ( getLibraryModuleModule
    , FetchModule
    , runFetchModule
    , directoryFetchModule
    , libraryFetchModule
    , textFetchModule
    ) where

import Pinafore.Language.DefDoc
import Pinafore.Language.Error
import Pinafore.Language.Grammar
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Defs
import Pinafore.Language.Name
import Pinafore.Language.Type
import Shapes
import System.Directory (doesFileExist)
import System.FilePath

newtype FetchModule context = MkFetchModule
    { runFetchModule :: context -> ModuleName -> QInterpreter (Maybe QModule)
    }

instance Semigroup (FetchModule context) where
    MkFetchModule fma <> MkFetchModule fmb =
        MkFetchModule $ \context mname -> do
            mrr <- fma context mname
            case mrr of
                Just rr -> return $ Just rr
                Nothing -> fmb context mname

instance Monoid (FetchModule context) where
    mempty = MkFetchModule $ \_ _ -> return Nothing

instance Contravariant FetchModule where
    contramap ab (MkFetchModule f) = MkFetchModule $ f . ab

loadModuleFromText :: ModuleName -> Text -> QInterpreter QModule
loadModuleFromText modname text =
    transformTMap (void $ interpretImportDeclaration stdModuleName) $ parseModule modname text

loadModuleFromByteString :: ModuleName -> LazyByteString -> QInterpreter QModule
loadModuleFromByteString modname bs =
    case eitherToResult $ decodeUtf8' $ toStrict bs of
        SuccessResult text -> loadModuleFromText modname text
        FailureResult err -> throw $ UnicodeDecodeError $ pack $ show err

textFetchModule :: (ModuleName -> IO (Maybe Text)) -> FetchModule context
textFetchModule getText =
    MkFetchModule $ \_ modname -> do
        mtext <- liftIO $ getText modname
        for mtext $ \text -> paramWith sourcePosParam (initialPos $ show modname) $ loadModuleFromText modname text

moduleRelativePath :: ModuleName -> FilePath
moduleRelativePath (MkModuleName nn) = (foldl1 (</>) $ fmap unpack nn) <> ".pinafore"

directoryFetchModule :: FilePath -> FetchModule context
directoryFetchModule dirpath =
    MkFetchModule $ \_ modname -> do
        let fpath = dirpath </> moduleRelativePath modname
        found <- liftIO $ doesFileExist fpath
        case found of
            False -> return Nothing
            True -> do
                bs <- liftIO $ readFile fpath
                mm <- paramWith sourcePosParam (initialPos fpath) $ loadModuleFromByteString modname bs
                return $ Just mm

getLibraryModuleModule :: forall context. context -> LibraryModule context -> QInterpreter QModule
getLibraryModuleModule context libmod = do
    let
        bindDocs :: [BindDoc context]
        bindDocs = libraryModuleEntries libmod
        seBinding :: ScopeEntry context -> Maybe (Name, QInterpreterBinding)
        seBinding (BindScopeEntry name mb) = do
            b <- mb
            return (name, b context)
        seBinding _ = Nothing
        getEntry :: BindDoc context -> Maybe (Name, DocInterpreterBinding QTypeSystem)
        getEntry MkBindDoc {..} = do
            (name, b) <- seBinding bdScopeEntry
            return (name, (docDescription bdDoc, b))
        bscope :: QScope
        bscope = bindingsScope $ mapFromList $ mapMaybe getEntry bindDocs
    dscopes <-
        for bindDocs $ \bd ->
            case bdScopeEntry bd of
                BindScopeEntry _ _ -> return emptyScope
                SubtypeScopeEntry entry -> getSubtypeScope entry
    let moduleDoc = libraryModuleDocumentation libmod
    moduleScope <- joinAllScopes dscopes bscope
    return $ MkModule {..}

libraryFetchModule :: forall context. [LibraryModule context] -> FetchModule context
libraryFetchModule lmods = let
    m :: Map Text (LibraryModule context)
    m = mapFromList $ fmap (\lmod -> (libraryModuleName lmod, lmod)) lmods
    in MkFetchModule $ \context mname -> for (lookup (toText mname) m) $ getLibraryModuleModule context
