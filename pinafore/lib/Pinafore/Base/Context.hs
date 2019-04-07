module Pinafore.Base.Context
    ( PinaforeContext
    , runPinaforeAction
    , pinaforeUndoActions
    , pinaforeCloseAllWindows
    , makePinaforeContext
    , nullPinaforeContext
    ) where

import Pinafore.Base.Action
import Shapes
import Truth.Core
import Truth.UI.GTK

data PinaforeContext baseedit =
    MkPinaforeContext (PinaforeAction baseedit () -> IO ())
                      UndoActions
                      (IO ())

runPinaforeAction :: (?pinafore :: PinaforeContext baseedit) => PinaforeAction baseedit () -> IO ()
runPinaforeAction =
    case ?pinafore of
        MkPinaforeContext unlift _ _ -> unlift

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
    let
        unlift (MkPinaforeAction action) = let
            openwin :: WindowSpec baseedit -> IO UIWindow
            openwin uiw = createWindow $ MkUserInterface sub uiw
            in do
                   _ <- getComposeM $ runReaderT action (openwin, subObject sub)
                   return ()
    return $ MkPinaforeContext unlift uactions closeAllWindows

nullPinaforeContext :: PinaforeContext baseedit
nullPinaforeContext = let
    nope :: IO a
    nope = fail "null Pinafore context"
    in MkPinaforeContext (\_ -> nope) (MkUndoActions nope nope) nope
