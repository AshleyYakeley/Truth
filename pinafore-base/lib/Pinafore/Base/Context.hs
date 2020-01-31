module Pinafore.Base.Context
    ( PinaforeContext
    , pinaforeBase
    , unliftPinaforeAction
    , unliftPinaforeActionOrFail
    , runPinaforeAction
    , makePinaforeContext
    , nullPinaforeContext
    , pinaforeBaseSubscriber
    ) where

import Pinafore.Base.Action
import Pinafore.Base.Know
import Pinafore.Base.Lens
import Shapes
import Truth.Core

data PinaforeContext baseupdate = MkPinaforeContext
    { pconRun :: forall a. PinaforeAction a -> IO (Know a)
    , pconBase :: Subscriber baseupdate
    }

unliftPinaforeAction :: (?pinafore :: PinaforeContext baseupdate) => PinaforeAction a -> IO (Know a)
unliftPinaforeAction = pconRun ?pinafore

unliftPinaforeActionOrFail :: (?pinafore :: PinaforeContext baseupdate) => PinaforeAction a -> IO a
unliftPinaforeActionOrFail action = do
    ka <- unliftPinaforeAction action
    case ka of
        Known a -> return a
        Unknown -> fail "action stopped"

runPinaforeAction :: (?pinafore :: PinaforeContext baseupdate) => PinaforeAction () -> IO ()
runPinaforeAction action = fmap (\_ -> ()) $ unliftPinaforeAction action

pinaforeBase :: (?pinafore :: PinaforeContext baseupdate) => Subscriber baseupdate
pinaforeBase = pconBase ?pinafore

makePinaforeContext ::
       forall baseupdate. InvertibleEdit (UpdateEdit baseupdate)
    => Subscriber baseupdate
    -> UIToolkit
    -> LifeCycleIO (PinaforeContext baseupdate)
makePinaforeContext rsub toolkit = do
    (sub, uactions) <- liftIO $ undoQueueSubscriber rsub
    return $ MkPinaforeContext (unPinaforeAction toolkit uactions) sub

nullPinaforeContext :: PinaforeContext baseupdate
nullPinaforeContext = let
    pconRun _ = fail "null Pinafore context"
    pconBase = error "no pinafore base"
    in MkPinaforeContext {..}

pinaforeBaseSubscriber ::
       forall baseupdate update. (?pinafore :: PinaforeContext baseupdate, BaseEditLens update baseupdate)
    => Subscriber update
pinaforeBaseSubscriber = mapSubscriber baseEditLens pinaforeBase
