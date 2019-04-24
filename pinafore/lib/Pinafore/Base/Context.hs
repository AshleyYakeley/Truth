module Pinafore.Base.Context
    ( PinaforeContext
    , unliftPinaforeAction
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

runPinaforeAction :: (?pinafore :: PinaforeContext baseedit) => PinaforeAction baseedit () -> IO ()
runPinaforeAction action = fmap (\_ -> ()) $ unliftPinaforeAction action

makePinaforeContext ::
       forall baseedit. InvertibleEdit baseedit
    => Bool
    -> Object baseedit
    -> UIToolkit
    -> LifeCycleIO (PinaforeContext baseedit)
makePinaforeContext async pinaforeObject toolkit = do
    rsub <- makeObjectSubscriber async pinaforeObject
    (sub, uactions) <- liftIO $ undoQueueSubscriber rsub
    return $ MkPinaforeContext $ unPinaforeAction toolkit sub uactions

nullPinaforeContext :: PinaforeContext baseedit
nullPinaforeContext = MkPinaforeContext $ \_ -> fail "null Pinafore context"
