module Pinafore.Language.Library.FetchModule
    ( getModuleScope
    , FetchModule
    , runFetchModule
    , directoryFetchModule
    , libraryFetchModule
    , textFetchModule
    ) where

import Pinafore.Context
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
                                PinaforeScope -> ModuleName -> PinaforeInterpreter (Maybe PinaforeScope)
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

loadModuleFromText :: PinaforeScope -> FilePath -> Text -> PinaforeInterpreter PinaforeScope
loadModuleFromText implictScope fpath text =
    importScope implictScope $ runSourcePos (initialPos fpath) $ parseModule text

loadModuleFromByteString :: PinaforeScope -> FilePath -> LazyByteString -> PinaforeInterpreter PinaforeScope
loadModuleFromByteString implictScope fpath bs =
    case eitherToResult $ decodeUtf8' $ toStrict bs of
        SuccessResult text -> loadModuleFromText implictScope fpath text
        FailureResult err -> throwErrorType (initialPos fpath) $ UnicodeDecodeError $ pack $ show err

textFetchModule :: (ModuleName -> IO (Maybe Text)) -> FetchModule
textFetchModule getText =
    MkFetchModule $ \implictScope mname -> do
        mtext <- liftIO $ getText mname
        for mtext $ \text -> loadModuleFromText implictScope (show mname) text

moduleRelativePath :: ModuleName -> FilePath
moduleRelativePath (MkModuleName nn) = (foldl1 (</>) $ fmap unpack nn) <> ".pinafore"

directoryFetchModule :: FilePath -> FetchModule
directoryFetchModule dirpath =
    MkFetchModule $ \implictScope mname -> do
        let fpath = dirpath </> moduleRelativePath mname
        found <- liftIO $ doesFileExist fpath
        case found of
            False -> return Nothing
            True -> do
                bs <- liftIO $ readFile fpath
                scope <- loadModuleFromByteString implictScope fpath bs
                return $ Just scope

getModuleScope :: (?pinafore :: PinaforeContext) => LibraryModule -> IO PinaforeScope
getModuleScope dt = do
    let
        bindDocs :: [BindDoc]
        bindDocs = toList dt
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
        seSubtype :: ScopeEntry -> [SubypeConversionEntry PinaforeGroundType]
        seSubtype (SubtypeScopeEntry entries) = entries
        seSubtype _ = []
    sscope <- getSubtypesScope $ mconcat $ fmap (seSubtype . bdScopeEntry) bindDocs
    return $ bscope <> sscope

libraryFetchModule :: [LibraryModule] -> FetchModule
libraryFetchModule lmods = let
    m :: Map Text LibraryModule
    m = mapFromList $ fmap (\lmod -> (docTreeName lmod, lmod)) lmods
    in MkFetchModule $ \_ mname -> liftIO $ for (lookup (toText mname) m) getModuleScope
