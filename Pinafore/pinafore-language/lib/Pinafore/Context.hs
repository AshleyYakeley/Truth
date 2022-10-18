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
    ) where

import Changes.Core
import Pinafore.Base
import Shapes

data InvocationInfo = MkInvocationInfo
    { iiScriptName :: String
    , iiScriptArguments :: [String]
    , iiEnvironment :: [(String, String)]
    , iiStdIn :: Source IO Text
    , iiStdOut :: Sink IO Text
    , iiStdErr :: Sink IO Text
    }

nullInvocationInfo :: InvocationInfo
nullInvocationInfo = let
    iiScriptName = ""
    iiScriptArguments = []
    iiEnvironment = []
    iiStdIn = nullSource
    iiStdOut = mempty
    iiStdErr = mempty
    in MkInvocationInfo {..}

data PinaforeContext = MkPinaforeContext
    { pconUnliftAction :: forall a. PinaforeAction a -> View (Know a)
    , pconStorageModel :: Model PinaforeStorageUpdate
    , pconInvocation :: InvocationInfo
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

makePinaforeContext :: InvocationInfo -> Model PinaforeStorageUpdate -> Lifecycle PinaforeContext
makePinaforeContext pconInvocation rmodel = do
    uh <- liftIO newUndoHandler
    let
        pconUnliftAction :: forall a. PinaforeAction a -> View (Know a)
        pconUnliftAction = unPinaforeAction uh
        pconStorageModel = undoHandlerModel uh rmodel
    return $ MkPinaforeContext {..}

nullPinaforeContext :: PinaforeContext
nullPinaforeContext = let
    pconUnliftAction _ = fail "null Pinafore context"
    pconStorageModel = error "no pinafore base"
    pconInvocation = nullInvocationInfo
    in MkPinaforeContext {..}
