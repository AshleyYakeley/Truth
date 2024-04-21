module Pinafore.Language.Library.FetchModule
    ( FetchModule
    , runFetchModule
    , directoryFetchModule
    , libraryFetchModule
    , textFetchModule
    , Importer(..)
    , getImporters
    ) where

import Pinafore.Language.DefDoc
import Pinafore.Language.Error
import Pinafore.Language.Grammar
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Defs
import Pinafore.Language.Name
import Pinafore.Text
import Shapes
import System.Directory (doesFileExist)
import System.FilePath

newtype FetchModule = MkFetchModule
    { runFetchModule :: ModuleName -> QInterpreter (Maybe QModule)
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

loadModuleFromText :: Text -> QInterpreter QModule
loadModuleFromText text = do
    sd <- interpretImportDeclaration $ PlainModuleSpec builtInModuleName
    withScopeDocs sd $ parseModule text

loadModuleFromByteString :: LazyByteString -> QInterpreter QModule
loadModuleFromByteString bs =
    case eitherToResult $ decodeUtf8' $ toStrict bs of
        SuccessResult text -> loadModuleFromText text
        FailureResult err -> throw $ UnicodeDecodeError $ showNamedText err

textFetchModule :: (ModuleName -> IO (Maybe Text)) -> FetchModule
textFetchModule getText =
    MkFetchModule $ \modname -> do
        mtext <- liftIO $ getText modname
        for mtext $ \text -> paramWith sourcePosParam (initialPos $ show modname) $ loadModuleFromText text

moduleRelativePath :: ModuleName -> FilePath
moduleRelativePath (MkModuleName t) = unpack $ t <> ".pinafore"

directoryFetchModule :: FilePath -> FetchModule
directoryFetchModule dirpath =
    MkFetchModule $ \modname -> do
        let fpath = dirpath </> moduleRelativePath modname
        found <- liftIO $ doesFileExist fpath
        case found of
            False -> return Nothing
            True -> do
                bs <- liftIO $ readFile fpath
                mm <- paramWith sourcePosParam (initialPos fpath) $ loadModuleFromByteString bs
                return $ Just mm

getLibraryContentsModule :: forall context. context -> LibraryStuff context -> QInterpreter QModule
getLibraryContentsModule context libmod = do
    let
        bindDocs :: [(ScopeEntry context, DefDoc)]
        bindDocs = mapMaybe (\MkBindDoc {..} -> fmap (\se -> (se, bdDoc)) bdScopeEntry) $ libraryContentsEntries libmod
        bscope :: QScope
        bscope =
            bindingInfosToScope $ do
                (se, bd) <- bindDocs
                case se of
                    (BindScopeEntry oname xnames b) -> do
                        let biOriginalName = namespaceConcatFullName RootNamespace oname
                        biName <- biOriginalName : xnames
                        let
                            biDocumentation = bd
                            biValue = b context
                        return (biName, MkQBindingInfo {..})
                    _ -> []
    dscopes <-
        for bindDocs $ \(se, _) ->
            case se of
                BindScopeEntry _ _ _ -> return emptyScope
                SubtypeScopeEntry entry -> getSubtypeScope entry
    scope <- joinAllScopes $ bscope : dscopes
    return $ MkQModule (libraryContentsDocumentation libmod) scope

libraryFetchModule :: forall context. context -> [LibraryModule context] -> FetchModule
libraryFetchModule context lmods = let
    m :: Map ModuleName (LibraryStuff context)
    m = mapFromList $ fmap (\MkLibraryModule {..} -> (lmName, lmContents)) lmods
    in MkFetchModule $ \mname -> for (lookup mname m) $ getLibraryContentsModule context

data Importer =
    MkImporter Name
               (Text -> ResultT Text IO (LibraryStuff ()))

processImporter :: ResultT Text IO (LibraryStuff ()) -> QInterpreter QModule
processImporter f = do
    ren <- liftIO $ tryExc $ runResultT f
    case ren of
        FailureResult err -> throw $ ImporterError $ toNamedText $ showText err
        SuccessResult (FailureResult err) -> throw $ ImporterError $ toNamedText err
        SuccessResult (SuccessResult lc) -> getLibraryContentsModule () lc

getImporters :: [Importer] -> Map Name (Text -> QInterpreter QModule)
getImporters ii = mapFromList $ fmap (\(MkImporter name f) -> (name, \t -> processImporter $ f t)) ii
