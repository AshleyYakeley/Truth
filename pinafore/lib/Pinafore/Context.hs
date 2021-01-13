module Pinafore.Context
    ( FetchModule(..)
    , directoryFetchModule
    , InvocationInfo(..)
    , nullInvocationInfo
    , PinaforeContext
    , unliftPinaforeAction
    , unliftPinaforeActionOrFail
    , runPinaforeAction
    , makePinaforeContext
    , nullPinaforeContext
    , pinaforeEntityModel
    , pinaforeInvocationInfo
    , pinaforeStdOut
    , pinaforeFetchModuleText
    ) where

import Changes.Core
import Pinafore.Base
import Pinafore.Language.Name
import Shapes
import System.Directory (doesFileExist)
import System.FilePath

newtype FetchModule = MkFetchModule
    { runFetchModule :: ModuleName -> IO (Maybe (FilePath, Result UnicodeException Text))
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

directoryFetchModule :: FilePath -> FetchModule
directoryFetchModule dirpath =
    MkFetchModule $ \mname -> do
        let fpath = dirpath </> moduleRelativePath mname
        found <- doesFileExist fpath
        case found of
            False -> return Nothing
            True -> do
                bs <- readFile fpath
                return $ Just $ (fpath, eitherToResult $ decodeUtf8' $ toStrict bs)

data InvocationInfo = MkInvocationInfo
    { iiScriptName :: String
    , iiScriptArguments :: [String]
    , iiEnvironment :: [(String, String)]
    }

nullInvocationInfo :: InvocationInfo
nullInvocationInfo = let
    iiScriptName = ""
    iiScriptArguments = []
    iiEnvironment = []
    in MkInvocationInfo {..}

data PinaforeContext = MkPinaforeContext
    { pconUnliftAction :: forall a. PinaforeAction a -> CreateView (Know a)
    , pconUnliftCreateView :: MFunction CreateView View
    , pconEntityModel :: Model PinaforeStorageUpdate
    , pconFetchModule :: FetchModule
    , pconInvocation :: InvocationInfo
    , pconStdOut :: Handle
    }

unliftPinaforeAction :: (?pinafore :: PinaforeContext) => PinaforeAction a -> CreateView (Know a)
unliftPinaforeAction = pconUnliftAction ?pinafore

unliftPinaforeActionOrFail :: (?pinafore :: PinaforeContext) => PinaforeAction a -> CreateView a
unliftPinaforeActionOrFail action = do
    ka <- unliftPinaforeAction action
    case ka of
        Known a -> return a
        Unknown -> fail "action stopped"

runPinaforeAction :: (?pinafore :: PinaforeContext) => PinaforeAction () -> View ()
runPinaforeAction action = pconUnliftCreateView ?pinafore $ fmap (\_ -> ()) $ unliftPinaforeAction action

pinaforeEntityModel :: (?pinafore :: PinaforeContext) => Model PinaforeStorageUpdate
pinaforeEntityModel = pconEntityModel ?pinafore

pinaforeInvocationInfo :: (?pinafore :: PinaforeContext) => InvocationInfo
pinaforeInvocationInfo = pconInvocation ?pinafore

pinaforeStdOut :: (?pinafore :: PinaforeContext) => Handle
pinaforeStdOut = pconStdOut ?pinafore

moduleRelativePath :: ModuleName -> FilePath
moduleRelativePath (MkModuleName nn) = (foldl1 (</>) $ fmap unpack nn) <> ".pinafore"

makePinaforeContext ::
       FetchModule
    -> InvocationInfo
    -> Handle
    -> Model PinaforeStorageUpdate
    -> ChangesContext
    -> LifeCycle PinaforeContext
makePinaforeContext pconFetchModule pconInvocation pconStdOut rmodel tc = do
    uh <- liftIO newUndoHandler
    let
        pconUnliftAction :: forall a. PinaforeAction a -> CreateView (Know a)
        pconUnliftAction = unPinaforeAction tc uh
        pconUnliftCreateView :: MFunction CreateView View
        pconUnliftCreateView = ccUnliftCreateView tc
        pconEntityModel = undoHandlerModel uh rmodel
    return $ MkPinaforeContext {..}

pinaforeFetchModuleText :: (?pinafore :: PinaforeContext) => FetchModule
pinaforeFetchModuleText = pconFetchModule ?pinafore

nullPinaforeContext :: PinaforeContext
nullPinaforeContext = let
    pconUnliftAction _ = fail "null Pinafore context"
    pconUnliftCreateView _ = fail "null Pinafore context"
    pconEntityModel = error "no pinafore base"
    pconFetchModule = mempty
    pconInvocation = nullInvocationInfo
    pconStdOut = stdout
    in MkPinaforeContext {..}
