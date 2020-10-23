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
    { pconUnliftAction :: forall a. PinaforeAction a -> CreateView (Know a)
    , pconUnliftCreateView :: MFunction CreateView View
    , pconEntityModel :: Model PinaforeStorageUpdate
    , pconInvocation :: InvocationInfo
    }

unliftPinaforeAction :: (?pinafore :: PinaforeContext) => PinaforeAction a -> CreateView (Know a)
unliftPinaforeAction = pconUnliftAction ?pinafore

unliftPinaforeActionOrFail :: (?pinafore :: PinaforeContext) => PinaforeAction a -> CreateView a
unliftPinaforeActionOrFail action = do
    ka <- unliftPinaforeAction action
    case ka of
        Known a -> return a
        Unknown -> fail "action stopped"

runPinaforeActionCV :: (?pinafore :: PinaforeContext) => PinaforeAction () -> CreateView ()
runPinaforeActionCV action = fmap (\_ -> ()) $ unliftPinaforeAction action

runPinaforeAction :: (?pinafore :: PinaforeContext) => PinaforeAction () -> View ()
runPinaforeAction action = pconUnliftCreateView ?pinafore $ runPinaforeActionCV action

pinaforeEntityModel :: (?pinafore :: PinaforeContext) => Model PinaforeStorageUpdate
pinaforeEntityModel = pconEntityModel ?pinafore

pinaforeInvocationInfo :: (?pinafore :: PinaforeContext) => InvocationInfo
pinaforeInvocationInfo = pconInvocation ?pinafore

makePinaforeContext :: InvocationInfo -> Model PinaforeStorageUpdate -> ChangesContext -> LifeCycleIO PinaforeContext
makePinaforeContext pconInvocation rmodel tc = do
    uh <- liftIO newUndoHandler
    let
        pconUnliftAction :: forall a. PinaforeAction a -> CreateView (Know a)
        pconUnliftAction = unPinaforeAction tc uh
        pconUnliftCreateView :: MFunction CreateView View
        pconUnliftCreateView = tcUnliftCreateView tc
        pconEntityModel = undoHandlerModel uh rmodel
    return $ MkPinaforeContext {..}

nullPinaforeContext :: PinaforeContext
nullPinaforeContext = let
    pconUnliftAction _ = fail "null Pinafore context"
    pconUnliftCreateView _ = fail "null Pinafore context"
    pconEntityModel = error "no pinafore base"
    pconInvocation = nullInvocationInfo
    in MkPinaforeContext {..}
