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

newtype PinaforeContext baseedit =
    MkPinaforeContext (forall a. PinaforeAction baseedit a -> IO (Know a))

unliftPinaforeAction :: (?pinafore :: PinaforeContext baseedit) => PinaforeAction baseedit a -> IO (Know a)
unliftPinaforeAction =
    case ?pinafore of
        MkPinaforeContext unlift -> unlift

unliftPinaforeActionOrFail :: (?pinafore :: PinaforeContext baseedit) => PinaforeAction baseedit a -> IO a
unliftPinaforeActionOrFail action = do
    ka <- unliftPinaforeAction action
    case ka of
        Known a -> return a
        Unknown -> fail "action stopped"

runPinaforeAction :: (?pinafore :: PinaforeContext baseedit) => PinaforeAction baseedit () -> IO ()
runPinaforeAction action = fmap (\_ -> ()) $ unliftPinaforeAction action

makePinaforeContext ::
       forall baseedit. InvertibleEdit baseedit
    => Subscriber baseedit
    -> UIToolkit
    -> LifeCycleIO (PinaforeContext baseedit)
makePinaforeContext rsub toolkit = do
    (sub, uactions) <- liftIO $ undoQueueSubscriber rsub
    return $ MkPinaforeContext $ unPinaforeAction toolkit sub uactions

nullPinaforeContext :: PinaforeContext baseedit
nullPinaforeContext = MkPinaforeContext $ \_ -> fail "null Pinafore context"
