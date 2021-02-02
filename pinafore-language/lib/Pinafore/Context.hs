module Pinafore.Context
    ( InvocationInfo(..)
    , nullInvocationInfo
    , PinaforeContext
    , unliftPinaforeAction
    , unliftPinaforeActionOrFail
    , runPinaforeAction
    , makePinaforeContext
    , nullPinaforeContext
    , pinaforeEntityModel
    , pinaforeFileModel
    , pinaforeInvocationInfo
    , pinaforeStdOut
    ) where

import Changes.Core
import Pinafore.Base
import Pinafore.Storage
import Shapes

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
    , pconFileModel :: Model PinaforeFileUpdate
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

pinaforeFileModel :: (?pinafore :: PinaforeContext) => Model PinaforeFileUpdate
pinaforeFileModel = pconFileModel ?pinafore

pinaforeInvocationInfo :: (?pinafore :: PinaforeContext) => InvocationInfo
pinaforeInvocationInfo = pconInvocation ?pinafore

pinaforeStdOut :: (?pinafore :: PinaforeContext) => Handle
pinaforeStdOut = pconStdOut ?pinafore

makePinaforeContext ::
       InvocationInfo
    -> Handle
    -> Model PinaforeStorageUpdate
    -> Model PinaforeFileUpdate
    -> ChangesContext
    -> LifeCycle PinaforeContext
makePinaforeContext pconInvocation pconStdOut rmodel pconFileModel tc = do
    uh <- liftIO newUndoHandler
    let
        pconUnliftAction :: forall a. PinaforeAction a -> CreateView (Know a)
        pconUnliftAction = unPinaforeAction tc uh
        pconUnliftCreateView :: MFunction CreateView View
        pconUnliftCreateView = ccUnliftCreateView tc
        pconEntityModel = undoHandlerModel uh rmodel
    return $ MkPinaforeContext {..}

nullPinaforeContext :: PinaforeContext
nullPinaforeContext = let
    pconUnliftAction _ = fail "null Pinafore context"
    pconUnliftCreateView _ = fail "null Pinafore context"
    pconEntityModel = error "no pinafore base"
    pconFileModel = error "no pinafore base"
    pconInvocation = nullInvocationInfo
    pconStdOut = stdout
    in MkPinaforeContext {..}
