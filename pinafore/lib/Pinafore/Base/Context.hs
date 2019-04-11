module Pinafore.Base.Context
    ( PinaforeContext
    , unliftPinaforeAction
    , runPinaforeAction
    , pinaforeUndoActions
    , makePinaforeContext
    , nullPinaforeContext
    ) where

import Pinafore.Base.Action
import Pinafore.Base.Know
import Shapes
import Truth.Core

data PinaforeContext baseedit =
    MkPinaforeContext (forall a. PinaforeAction baseedit a -> IO (Know a))
                      UndoActions

unliftPinaforeAction :: (?pinafore :: PinaforeContext baseedit) => PinaforeAction baseedit a -> IO (Know a)
unliftPinaforeAction =
    case ?pinafore of
        MkPinaforeContext unlift _ -> unlift

runPinaforeAction :: (?pinafore :: PinaforeContext baseedit) => PinaforeAction baseedit () -> IO ()
runPinaforeAction action = fmap (\_ -> ()) $ unliftPinaforeAction action

pinaforeUndoActions :: (?pinafore :: PinaforeContext baseedit) => UndoActions
pinaforeUndoActions =
    case ?pinafore of
        MkPinaforeContext _ uactions -> uactions

makePinaforeContext ::
       forall baseedit. InvertibleEdit baseedit
    => Bool
    -> Object baseedit
    -> UIToolkit
    -> LifeCycle (PinaforeContext baseedit)
makePinaforeContext async pinaforeObject toolkit = do
    rsub <- liftIO $ makeObjectSubscriber async pinaforeObject
    (sub, uactions) <- liftIO $ undoQueueSubscriber rsub
    return $ MkPinaforeContext (unPinaforeAction toolkit sub) uactions

nullPinaforeContext :: PinaforeContext baseedit
nullPinaforeContext = let
    nope :: IO a
    nope = fail "null Pinafore context"
    in MkPinaforeContext (\_ -> nope) (MkUndoActions (\_ -> nope) (\_ -> nope))
