module Pinafore.Base.Context
    ( PinaforeContext
    , unliftPinaforeAction
    , unliftPinaforeActionOrFail
    , runPinaforeAction
    , makePinaforeContext
    , nullPinaforeContext
    , pinaforeEntityModel
    ) where

import Pinafore.Base.Action
import Pinafore.Base.Edit
import Pinafore.Base.Know
import Shapes
import Truth.Core

data PinaforeContext = MkPinaforeContext
    { pconRun :: forall a. PinaforeAction a -> View (Know a)
    , pconEntityModel :: Model PinaforeEntityUpdate
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

pinaforeEntityModel :: (?pinafore :: PinaforeContext) => Model PinaforeEntityUpdate
pinaforeEntityModel = pconEntityModel ?pinafore

makePinaforeContext :: Model PinaforeEntityUpdate -> UIToolkit -> LifeCycleIO PinaforeContext
makePinaforeContext rmodel toolkit = do
    uh <- liftIO newUndoHandler
    return $ MkPinaforeContext (unPinaforeAction toolkit uh) $ undoHandlerModel uh rmodel

nullPinaforeContext :: PinaforeContext
nullPinaforeContext = let
    pconRun _ = fail "null Pinafore context"
    pconEntityModel = error "no pinafore base"
    in MkPinaforeContext {..}
