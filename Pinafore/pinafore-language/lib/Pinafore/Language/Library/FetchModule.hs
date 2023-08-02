module Pinafore.Language.Library.FetchModule
    ( FetchModule
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
loadModuleFromText modname text = do
    sd <- interpretImportDeclaration builtInModuleName
    withScopeDocs sd $ parseModule modname text

loadModuleFromByteString :: ModuleName -> LazyByteString -> QInterpreter QModule
loadModuleFromByteString modname bs =
    case eitherToResult $ decodeUtf8' $ toStrict bs of
        SuccessResult text -> loadModuleFromText modname text
        FailureResult err -> throw $ UnicodeDecodeError $ showNamedText err

textFetchModule :: (ModuleName -> IO (Maybe Text)) -> FetchModule context
textFetchModule getText =
    MkFetchModule $ \_ modname -> do
        mtext <- liftIO $ getText modname
        for mtext $ \text -> paramWith sourcePosParam (initialPos $ show modname) $ loadModuleFromText modname text

moduleRelativePath :: ModuleName -> FilePath
moduleRelativePath (MkModuleName t) = unpack $ t <> ".pinafore"

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
        bindDocs :: [(ScopeEntry context, DefDoc)]
        bindDocs = mapMaybe (\MkBindDoc {..} -> fmap (\se -> (se, bdDoc)) bdScopeEntry) $ libraryModuleEntries libmod
        bscope :: QScope
        bscope =
            bindingInfosToScope $ do
                (se, bd) <- bindDocs
                case se of
                    (BindScopeEntry oname xnames b) -> do
                        let biOriginalName = namespaceConcatFullName RootNamespace oname
                        biName <- biOriginalName : xnames
                        let
                            biDocumentation = docDescription bd
                            biValue = b context
                        return (biName, MkQBindingInfo {..})
                    _ -> []
    dscopes <-
        for bindDocs $ \(se, _) ->
            case se of
                BindScopeEntry _ _ _ -> return emptyScope
                SubtypeScopeEntry entry -> getSubtypeScope entry
    scope <- joinAllScopes $ bscope : dscopes
    return $ MkQModule (libraryModuleDocumentation libmod) scope

libraryFetchModule :: forall context. [LibraryModule context] -> FetchModule context
libraryFetchModule lmods = let
    m :: Map ModuleName (LibraryModule context)
    m = mapFromList $ fmap (\libmod -> (lmName libmod, libmod)) lmods
    in MkFetchModule $ \context mname -> for (lookup mname m) $ getLibraryModuleModule context
