module Pinafore.Language.Library.LoadModule
    ( directoryLoadModule
    , libraryLoadModule
    , textLoadModule
    ) where

import Import
import Pinafore.Language.Error
import Pinafore.Language.Interpret
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.LibraryModule
import System.Directory (doesFileExist)
import System.FilePath

loadModuleFromText :: Text -> QInterpreter QModule
loadModuleFromText text = do
    sd <- interpretImportDeclaration builtInModuleName
    withScopeDocs sd $ parseModule text

loadModuleFromByteString :: LazyByteString -> QInterpreter QModule
loadModuleFromByteString bs =
    case eitherToResult $ decodeUtf8' $ toStrict bs of
        SuccessResult text -> loadModuleFromText text
        FailureResult err -> throw $ UnicodeDecodeError $ showNamedText err

textLoadModule :: (ModuleName -> IO (Maybe Text)) -> LoadModule
textLoadModule getText =
    MkLoadModule $ \modname -> do
        mtext <- liftIO $ getText modname
        for mtext $ \text -> paramWith sourcePosParam (initialPos $ show modname) $ loadModuleFromText text

moduleRelativePath :: ModuleName -> FilePath
moduleRelativePath (MkModuleName t) = unpack $ t <> ".pinafore"

directoryLoadModule :: FilePath -> LoadModule
directoryLoadModule dirpath =
    MkLoadModule $ \modname -> do
        let fpath = dirpath </> moduleRelativePath modname
        found <- liftIO $ doesFileExist fpath
        case found of
            False -> return Nothing
            True -> do
                bs <- liftIO $ readFile fpath
                mm <- paramWith sourcePosParam (initialPos fpath) $ loadModuleFromByteString bs
                return $ Just mm

getLibraryContentsModule :: LibraryStuff -> QInterpreter QModule
getLibraryContentsModule libmod = do
    let
        bindDocs :: [(ScopeEntry, DefDoc)]
        bindDocs = mapMaybe (\MkBindDoc {..} -> fmap (\se -> (se, bdDoc)) bdScopeEntry) $ libraryContentsEntries libmod
        bscope :: QScope
        bscope =
            bindingInfosToScope $ do
                (se, biDocumentation) <- bindDocs
                case se of
                    (BindScopeEntry oname xnames biValue) -> do
                        let biOriginalName = namespaceConcatFullName RootNamespace oname
                        biName <- biOriginalName : xnames
                        return (biName, MkQBindingInfo {..})
                    _ -> []
    dscopes <-
        for bindDocs $ \(se, _) ->
            case se of
                BindScopeEntry _ _ _ -> return emptyScope
                SubtypeScopeEntry entry -> getSubtypeScope entry
    scope <- joinAllScopes $ bscope : dscopes
    return $ MkQModule (libraryContentsDocumentation libmod) scope

libraryLoadModule :: [LibraryModule] -> LoadModule
libraryLoadModule lmods = let
    m :: Map ModuleName LibraryStuff
    m = mapFromList $ fmap (\MkLibraryModule {..} -> (lmName, lmContents)) lmods
    in MkLoadModule $ \mname -> for (lookup mname m) getLibraryContentsModule
