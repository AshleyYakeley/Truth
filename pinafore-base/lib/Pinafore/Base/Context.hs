module Pinafore.Base.Context
    ( PinaforeContext
    , pinaforeBase
    , unliftPinaforeAction
    , unliftPinaforeActionOrFail
    , runPinaforeAction
    , makePinaforeContext
    , nullPinaforeContext
    , pinaforeBaseModel
    ) where

import Pinafore.Base.Action
import Pinafore.Base.Know
import Pinafore.Base.Lens
import Shapes
import Truth.Core

data PinaforeContext baseupdate = MkPinaforeContext
    { pconRun :: forall a. PinaforeAction a -> View (Know a)
    , pconBase :: Model baseupdate
    }

unliftPinaforeAction :: (?pinafore :: PinaforeContext baseupdate) => PinaforeAction a -> View (Know a)
unliftPinaforeAction = pconRun ?pinafore

unliftPinaforeActionOrFail :: (?pinafore :: PinaforeContext baseupdate) => PinaforeAction a -> View a
unliftPinaforeActionOrFail action = do
    ka <- unliftPinaforeAction action
    case ka of
        Known a -> return a
        Unknown -> fail "action stopped"

runPinaforeAction :: (?pinafore :: PinaforeContext baseupdate) => PinaforeAction () -> View ()
runPinaforeAction action = fmap (\_ -> ()) $ unliftPinaforeAction action

pinaforeBase :: (?pinafore :: PinaforeContext baseupdate) => Model baseupdate
pinaforeBase = pconBase ?pinafore

makePinaforeContext ::
       forall baseupdate. InvertibleEdit (UpdateEdit baseupdate)
    => Model baseupdate
    -> UIToolkit
    -> LifeCycleIO (PinaforeContext baseupdate)
makePinaforeContext rsub toolkit = do
    (sub, uactions) <- liftIO $ undoQueueModel rsub
    return $ MkPinaforeContext (unPinaforeAction toolkit uactions) sub

nullPinaforeContext :: PinaforeContext baseupdate
nullPinaforeContext = let
    pconRun _ = fail "null Pinafore context"
    pconBase = error "no pinafore base"
    in MkPinaforeContext {..}

pinaforeBaseModel ::
       forall baseupdate update. (?pinafore :: PinaforeContext baseupdate, BaseEditLens update baseupdate)
    => Model update
pinaforeBaseModel = mapModel baseEditLens pinaforeBase
