module Pinafore.Context
    ( InvocationInfo(..)
    , nullInvocationInfo
    , PinaforeContext
    , unliftPinaforeAction
    , unliftPinaforeActionOrFail
    , runPinaforeAction
    , makePinaforeContext
    , nullPinaforeContext
    , pinaforeStorageModel
    , pinaforeInvocationInfo
    , pinaforeStdOut
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
    { pconUnliftAction :: forall a. PinaforeAction a -> View (Know a)
    , pconStorageModel :: Model PinaforeStorageUpdate
    , pconInvocation :: InvocationInfo
    , pconStdOut :: Handle
    }

unliftPinaforeAction :: (?pinafore :: PinaforeContext) => PinaforeAction a -> View (Know a)
unliftPinaforeAction = pconUnliftAction ?pinafore

unliftPinaforeActionOrFail :: (?pinafore :: PinaforeContext) => PinaforeAction --> View
unliftPinaforeActionOrFail action = do
    ka <- unliftPinaforeAction action
    case ka of
        Known a -> return a
        Unknown -> fail "action stopped"

runPinaforeAction :: (?pinafore :: PinaforeContext) => PinaforeAction () -> View ()
runPinaforeAction action = fmap (\_ -> ()) $ unliftPinaforeAction action

pinaforeStorageModel :: (?pinafore :: PinaforeContext) => Model PinaforeStorageUpdate
pinaforeStorageModel = pconStorageModel ?pinafore

pinaforeInvocationInfo :: (?pinafore :: PinaforeContext) => InvocationInfo
pinaforeInvocationInfo = pconInvocation ?pinafore

pinaforeStdOut :: (?pinafore :: PinaforeContext) => Handle
pinaforeStdOut = pconStdOut ?pinafore

makePinaforeContext ::
       InvocationInfo -> Handle -> Model PinaforeStorageUpdate -> ChangesContext -> LifeCycle PinaforeContext
makePinaforeContext pconInvocation pconStdOut rmodel tc = do
    uh <- liftIO newUndoHandler
    let
        pconUnliftAction :: forall a. PinaforeAction a -> View (Know a)
        pconUnliftAction = unPinaforeAction tc uh
        pconStorageModel = undoHandlerModel uh rmodel
    return $ MkPinaforeContext {..}

nullPinaforeContext :: PinaforeContext
nullPinaforeContext = let
    pconUnliftAction _ = fail "null Pinafore context"
    pconStorageModel = error "no pinafore base"
    pconInvocation = nullInvocationInfo
    pconStdOut = stdout
    in MkPinaforeContext {..}
