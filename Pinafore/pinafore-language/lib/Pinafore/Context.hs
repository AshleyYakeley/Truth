module Pinafore.Context
    ( InvocationInfo(..)
    , nullInvocationInfo
    , QContext
    , unliftAction
    , unliftActionOrFail
    , runAction
    , makeQContext
    , nullQContext
    , qStorageModel
    , qInvocationInfo
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

data QContext = MkQContext
    { pconUnliftAction :: forall a. Action a -> View (Know a)
    , pconStorageModel :: Model QStorageUpdate
    , pconInvocation :: InvocationInfo
    }

unliftAction :: (?qcontext :: QContext) => Action a -> View (Know a)
unliftAction = pconUnliftAction ?qcontext

unliftActionOrFail :: (?qcontext :: QContext) => Action --> View
unliftActionOrFail action = do
    ka <- unliftAction action
    case ka of
        Known a -> return a
        Unknown -> fail "action stopped"

runAction :: (?qcontext :: QContext) => Action () -> View ()
runAction action = fmap (\_ -> ()) $ unliftAction action

qStorageModel :: (?qcontext :: QContext) => Model QStorageUpdate
qStorageModel = pconStorageModel ?qcontext

qInvocationInfo :: (?qcontext :: QContext) => InvocationInfo
qInvocationInfo = pconInvocation ?qcontext

makeQContext :: InvocationInfo -> Model QStorageUpdate -> Lifecycle QContext
makeQContext pconInvocation rmodel = do
    uh <- liftIO newUndoHandler
    let
        pconUnliftAction :: forall a. Action a -> View (Know a)
        pconUnliftAction = unAction uh
        pconStorageModel = undoHandlerModel uh rmodel
    return $ MkQContext {..}

nullQContext :: QContext
nullQContext = let
    pconUnliftAction _ = fail "null Pinafore context"
    pconStorageModel = error "no pinafore base"
    pconInvocation = nullInvocationInfo
    in MkQContext {..}
