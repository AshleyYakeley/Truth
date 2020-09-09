module Pinafore.Base.Context
    ( PinaforeContext
    , unliftPinaforeAction
    , unliftPinaforeActionOrFail
    , runPinaforeAction
    , makePinaforeContext
    , nullPinaforeContext
    , pinaforeEntityModel
    ) where

import Changes.Core
import Pinafore.Base.Action
import Pinafore.Base.Edit
import Pinafore.Base.Know
import Shapes

data PinaforeContext = MkPinaforeContext
    { pconRun :: forall a. PinaforeAction a -> View (Know a)
    , pconEntityModel :: Model PinaforeStorageUpdate
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

makePinaforeContext :: Model PinaforeStorageUpdate -> ChangesContext -> LifeCycleIO PinaforeContext
makePinaforeContext rmodel tc = do
    uh <- liftIO newUndoHandler
    return $ MkPinaforeContext (unPinaforeAction tc uh) $ undoHandlerModel uh rmodel

nullPinaforeContext :: PinaforeContext
nullPinaforeContext = let
    pconRun _ = fail "null Pinafore context"
    pconEntityModel = error "no pinafore base"
    in MkPinaforeContext {..}
