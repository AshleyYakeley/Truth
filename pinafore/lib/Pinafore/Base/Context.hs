module Pinafore.Base.Context
    ( PinaforeContext
    , unliftPinaforeAction
    , runPinaforeAction
    , pinaforeUndoActions
    , pinaforeCloseAllWindows
    , makePinaforeContext
    , nullPinaforeContext
    ) where

import Pinafore.Base.Action
import Pinafore.Base.Know
import Shapes
import Truth.Core
import Truth.UI.GTK

data PinaforeContext baseedit =
    MkPinaforeContext (forall a. PinaforeAction baseedit a -> IO (Know a))
                      UndoActions
                      (IO ())

unliftPinaforeAction :: (?pinafore :: PinaforeContext baseedit) => PinaforeAction baseedit a -> IO (Know a)
unliftPinaforeAction =
    case ?pinafore of
        MkPinaforeContext unlift _ _ -> unlift

runPinaforeAction :: (?pinafore :: PinaforeContext baseedit) => PinaforeAction baseedit () -> IO ()
runPinaforeAction action = fmap (\_ -> ()) $ unliftPinaforeAction action

pinaforeUndoActions :: (?pinafore :: PinaforeContext baseedit) => UndoActions
pinaforeUndoActions =
    case ?pinafore of
        MkPinaforeContext _ uactions _ -> uactions

pinaforeCloseAllWindows :: (?pinafore :: PinaforeContext baseedit) => IO ()
pinaforeCloseAllWindows =
    case ?pinafore of
        MkPinaforeContext _ _ caw -> caw

makePinaforeContext ::
       forall baseedit. InvertibleEdit baseedit
    => Bool
    -> Object baseedit
    -> (UserInterface WindowSpec -> IO UIWindow)
    -> IO ()
    -> LifeCycle (PinaforeContext baseedit)
makePinaforeContext async pinaforeObject createWindow closeAllWindows = do
    rsub <- liftIO $ makeObjectSubscriber async pinaforeObject
    (sub, uactions) <- liftIO $ undoQueueSubscriber rsub
    return $ MkPinaforeContext (unPinaforeAction createWindow sub) uactions closeAllWindows

nullPinaforeContext :: PinaforeContext baseedit
nullPinaforeContext = let
    nope :: IO a
    nope = fail "null Pinafore context"
    in MkPinaforeContext (\_ -> nope) (MkUndoActions (\_ -> nope) (\_ -> nope)) nope
