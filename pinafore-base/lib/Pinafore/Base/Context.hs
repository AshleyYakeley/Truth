module Pinafore.Base.Context
    ( PinaforeContext(..)
    , pinaforeSubEntity
    , unliftPinaforeAction
    , unliftPinaforeActionOrFail
    , runPinaforeAction
    , nullPinaforeContext
    ) where

import Data.Time
import Pinafore.Base.Action
import Pinafore.Base.Edit
import Pinafore.Base.Know
import Shapes
import Truth.Core

data PinaforeContext = MkPinaforeContext
    { pconRun :: forall a. PinaforeAction a -> IO (Know a)
    , pconSubEntity :: Subscriber PinaforeEntityUpdate
    , pconSubTime :: Subscriber (ROWUpdate UTCTime)
    , pconSubTimeZone :: Subscriber (ROWUpdate TimeZone)
    }

unliftPinaforeAction :: (?pinafore :: PinaforeContext) => PinaforeAction a -> IO (Know a)
unliftPinaforeAction = pconRun ?pinafore

unliftPinaforeActionOrFail :: (?pinafore :: PinaforeContext) => PinaforeAction a -> IO a
unliftPinaforeActionOrFail action = do
    ka <- unliftPinaforeAction action
    case ka of
        Known a -> return a
        Unknown -> fail "action stopped"

runPinaforeAction :: (?pinafore :: PinaforeContext) => PinaforeAction () -> IO ()
runPinaforeAction action = fmap (\_ -> ()) $ unliftPinaforeAction action

pinaforeSubEntity :: (?pinafore :: PinaforeContext) => Subscriber PinaforeEntityUpdate
pinaforeSubEntity = pconSubEntity ?pinafore

{-
makePinaforeContext ::
       Subscriber PinaforeEntityUpdate
    -> UIToolkit
    -> LifeCycleIO PinaforeContext
makePinaforeContext rsub toolkit = do
    (sub, uactions) <- liftIO $ undoQueueSubscriber rsub
    return $ MkPinaforeContext (unPinaforeAction toolkit uactions) sub
-}
nullPinaforeContext :: PinaforeContext
nullPinaforeContext = let
    pconRun _ = fail "null Pinafore context"
    pconSubEntity = error "no pinafore base"
    pconSubTime = error "no pinafore base"
    pconSubTimeZone = error "no pinafore base"
    in MkPinaforeContext {..}
