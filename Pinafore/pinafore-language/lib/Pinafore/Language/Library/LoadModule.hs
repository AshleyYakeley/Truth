module Pinafore.Language.Library.LoadModule
    ( directoryLoadModule
    , libraryLoadModule
    , textLoadModule
    )
where

import System.Directory (doesFileExist)
import System.FilePath

import Import
import Pinafore.Language.Error
import Pinafore.Language.Interpret
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.LibraryModule

loadModuleFromText :: Text -> QInterpreter QModule
loadModuleFromText text = do
    sd <- interpretImportDeclaration builtInModuleName
    withDeclarations sd $ parseModule text

loadModuleFromByteString :: LazyByteString -> QInterpreter QModule
loadModuleFromByteString bs =
    case eitherToResult $ decodeUtf8' $ toStrict bs of
        SuccessResult text -> loadModuleFromText text
        FailureResult err -> throw $ UnicodeDecodeError $ showNamedText err

textLoadModule :: (ModuleName -> IO (Maybe Text)) -> LoadModule
textLoadModule getText =
    mempty
        { runLoadModule = \modname -> do
            mtext <- liftIO $ getText modname
            for mtext $ \text -> paramWith sourcePosParam (initialPos $ show modname) $ loadModuleFromText text
        }

moduleRelativePath :: ModuleName -> FilePath
moduleRelativePath (MkModuleName t) = unpack $ t <> ".pinafore"

directoryFindFile :: FilePath -> FilePath -> IO (Maybe (FilePath, LazyByteString))
directoryFindFile dirpath name = do
    let fpath = dirpath </> name
    found <- doesFileExist fpath
    case found of
        False -> return Nothing
        True -> do
            bs <- readFile fpath
            return $ Just (fpath, bs)

directoryLoadModule :: FilePath -> LoadModule
directoryLoadModule dirpath = let
    runLoadModule :: ModuleName -> QInterpreter (Maybe QModule)
    runLoadModule modname = do
        mbs <- liftIO $ directoryFindFile dirpath (moduleRelativePath modname)
        for mbs $ \(fpath, bs) ->
            paramWith sourcePosParam (initialPos fpath) $ loadModuleFromByteString bs
    getResourceLoadModule :: Text -> QInterpreter (Maybe LazyByteString)
    getResourceLoadModule path = do
        mbs <- liftIO $ directoryFindFile dirpath (unpack path)
        return $ fmap snd mbs
    in MkLoadModule{..}

getLibraryContentsModule :: LibraryStuff -> QInterpreter QModule
getLibraryContentsModule libmod = do
    let
        bindDocs :: [(ScopeEntry, DefDoc)]
        bindDocs = mapMaybe (\MkBindDoc{..} -> fmap (\se -> (se, bdDoc)) bdScopeEntry) $ libraryContentsEntries libmod
        bscope :: QScope
        bscope =
            bindingInfosToScope $ do
                (se, siDocumentation) <- bindDocs
                case se of
                    (BindScopeEntry oname xnames siItem) -> do
                        let siOriginalName = namespaceConcatFullName RootNamespace oname
                        biName <- siOriginalName : xnames
                        return (biName, MkQScopeItem{..})
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
    m = mapFromList $ fmap (\MkLibraryModule{..} -> (lmName, lmContents)) lmods
    in mempty
        { runLoadModule = \mname -> for (lookup mname m) getLibraryContentsModule
        }
