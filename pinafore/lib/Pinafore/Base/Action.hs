module Pinafore.Base.Action
    ( resultTextToM
    , PinaforeAction
    , pinaforeFunctionValueGet
    , pinaforeLensPush
    , pinaforeNewWindow
    , PinaforeContext
    , runPinaforeAction
    , makePinaforeContext
    , nullPinaforeContext
    ) where

import Pinafore.Base.Morphism
import Shapes
import Truth.Core
import Truth.UI.GTK

newtype PinaforeAction baseedit a =
    MkPinaforeAction (ReaderT (UIWindow baseedit -> IO (), Object baseedit) IO a)
    deriving (Functor, Applicative, Monad, MonadFail, MonadFix, MonadIO, MonadTunnelIO, MonadUnliftIO, MonadAskUnliftIO)

resultTextToM :: MonadFail m => Result Text a -> m a
resultTextToM = resultToM . mapResultFailure unpack

pinaforeFunctionValueGet :: PinaforeFunctionValue baseedit t -> PinaforeAction baseedit t
pinaforeFunctionValueGet fval = do
    (_, MkObject {..}) <- MkPinaforeAction ask
    liftIO $ runTransform objRun $ editFunctionRead fval objRead ReadWhole

pinaforeLensPush :: PinaforeLensValue baseedit edit -> [edit] -> PinaforeAction baseedit ()
pinaforeLensPush lens edits = do
    (_, object) <- MkPinaforeAction ask
    case lensObject True lens object of
        MkObject {..} -> liftIO $ runTransform objRun $ pushEdit $ objEdit edits

pinaforeNewWindow :: UIWindow baseedit -> PinaforeAction baseedit ()
pinaforeNewWindow uiw = do
    (neww, _) <- MkPinaforeAction ask
    liftIO $ neww uiw

newtype PinaforeContext baseedit =
    MkPinaforeContext (UnliftIO (PinaforeAction baseedit))

runPinaforeAction :: (?pinafore :: PinaforeContext baseedit) => PinaforeAction baseedit a -> IO a
runPinaforeAction action =
    case ?pinafore of
        MkPinaforeContext unlift -> runTransform unlift action

makePinaforeContext ::
       forall baseedit. Object baseedit -> (UserInterface UIWindow -> IO ()) -> LifeCycle (PinaforeContext baseedit)
makePinaforeContext pinaforeObject createWindow = do
    sub <- liftIO $ makeObjectSubscriber pinaforeObject
    (_, obj, _) <- subscribe sub (\_ -> return ()) (\_ _ _ -> return ())
    return $
        MkPinaforeContext $
        MkTransform $ \(MkPinaforeAction action :: PinaforeAction baseedit a) -> let
            openwin :: UIWindow baseedit -> IO ()
            openwin uiw = createWindow $ MkUserInterface sub uiw
            in runReaderT action (openwin, obj)

nullPinaforeContext :: PinaforeContext baseedit
nullPinaforeContext = MkPinaforeContext $ MkTransform $ \_ -> fail "null Pinafore context"
