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
    , pinaforeInvocationInfo
    ) where

import Changes.Core
import Pinafore.Base
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
    { pconRun :: forall a. PinaforeAction a -> View (Know a)
    , pconEntityModel :: Model PinaforeStorageUpdate
    , pconInvocation :: InvocationInfo
    }

unliftPinaforeAction :: (?pinafore :: PinaforeContext) => PinaforeAction a -> View (Know a)
unliftPinaforeAction = pconRun ?pinafore

unliftPinaforeActionOrFail :: (?pinafore :: PinaforeContext) => PinaforeAction a -> View a
unliftPinaforeActionOrFail action = do
    ka <- unliftPinaforeAction action
    case ka of
        Known a -> return a
        Unknown -> fail "action stopped"

runPinaforeAction :: (?pinafore :: PinaforeContext) => PinaforeAction () -> View ()
runPinaforeAction action = fmap (\_ -> ()) $ unliftPinaforeAction action

pinaforeEntityModel :: (?pinafore :: PinaforeContext) => Model PinaforeStorageUpdate
pinaforeEntityModel = pconEntityModel ?pinafore

pinaforeInvocationInfo :: (?pinafore :: PinaforeContext) => InvocationInfo
pinaforeInvocationInfo = pconInvocation ?pinafore

makePinaforeContext :: InvocationInfo -> Model PinaforeStorageUpdate -> ChangesContext -> LifeCycleIO PinaforeContext
makePinaforeContext pconInvocation rmodel tc = do
    uh <- liftIO newUndoHandler
    let
        pconRun :: forall a. PinaforeAction a -> View (Know a)
        pconRun = unPinaforeAction tc uh
        pconEntityModel = undoHandlerModel uh rmodel
    return $ MkPinaforeContext {..}

nullPinaforeContext :: PinaforeContext
nullPinaforeContext = let
    pconRun _ = fail "null Pinafore context"
    pconEntityModel = error "no pinafore base"
    pconInvocation = nullInvocationInfo
    in MkPinaforeContext {..}
