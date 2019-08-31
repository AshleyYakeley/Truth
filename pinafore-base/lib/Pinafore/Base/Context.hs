module Pinafore.Base.Context
    ( PinaforeContext
    , unliftPinaforeAction
    , unliftPinaforeActionOrFail
    , runPinaforeAction
    , makePinaforeContext
    , nullPinaforeContext
    ) where

import Pinafore.Base.Action
import Pinafore.Base.Know
import Shapes
import Truth.Core

newtype PinaforeContext baseupdate =
    MkPinaforeContext (forall a. PinaforeAction baseupdate a -> IO (Know a))

unliftPinaforeAction :: (?pinafore :: PinaforeContext baseupdate) => PinaforeAction baseupdate a -> IO (Know a)
unliftPinaforeAction =
    case ?pinafore of
        MkPinaforeContext unlift -> unlift

unliftPinaforeActionOrFail :: (?pinafore :: PinaforeContext baseupdate) => PinaforeAction baseupdate a -> IO a
unliftPinaforeActionOrFail action = do
    ka <- unliftPinaforeAction action
    case ka of
        Known a -> return a
        Unknown -> fail "action stopped"

runPinaforeAction :: (?pinafore :: PinaforeContext baseupdate) => PinaforeAction baseupdate () -> IO ()
runPinaforeAction action = fmap (\_ -> ()) $ unliftPinaforeAction action

makePinaforeContext ::
       forall baseupdate. InvertibleEdit (UpdateEdit baseupdate)
    => Subscriber baseupdate
    -> UIToolkit
    -> LifeCycleIO (PinaforeContext baseupdate)
makePinaforeContext rsub toolkit = do
    (sub, uactions) <- liftIO $ undoQueueSubscriber rsub
    return $ MkPinaforeContext $ unPinaforeAction toolkit sub uactions

nullPinaforeContext :: PinaforeContext baseupdate
nullPinaforeContext = MkPinaforeContext $ \_ -> fail "null Pinafore context"
