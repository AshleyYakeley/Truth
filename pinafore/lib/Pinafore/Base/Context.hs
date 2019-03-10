module Pinafore.Base.Context
    ( PinaforeContext
    , runPinaforeAction
    , pinaforeUndoActions
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

runPinaforeAction :: (?pinafore :: PinaforeContext baseedit) => PinaforeAction baseedit () -> IO ()
runPinaforeAction =
    case ?pinafore of
        MkPinaforeContext unlift _ -> unlift

pinaforeUndoActions :: (?pinafore :: PinaforeContext baseedit) => UndoActions
pinaforeUndoActions =
    case ?pinafore of
        MkPinaforeContext _ uactions -> uactions

makePinaforeContext ::
       forall baseedit. InvertibleEdit baseedit
    => Object baseedit
    -> (UserInterface WindowSpec -> IO UIWindow)
    -> LifeCycle (PinaforeContext baseedit)
makePinaforeContext pinaforeObject createWindow = do
    rsub <- liftIO $ makeObjectSubscriber pinaforeObject
    (sub, uactions) <- liftIO $ undoQueueSubscriber rsub
    let
        unlift (MkPinaforeAction action) = let
            openwin :: WindowSpec baseedit -> IO UIWindow
            openwin uiw = createWindow $ MkUserInterface sub uiw
            in do
                   _ <- getComposeM $ runReaderT action (openwin, subObject sub)
                   return ()
    return $ MkPinaforeContext unlift uactions

nullPinaforeContext :: PinaforeContext baseedit
nullPinaforeContext =
    MkPinaforeContext
        (\_ -> fail "null Pinafore context")
        (MkUndoActions (fail "null Pinafore context") (fail "null Pinafore context"))
