module Pinafore.Base.Action
    ( resultTextToM
    , PinaforeAction
    , pinaforeActionKnow
    , knowPinaforeAction
    , pinaforeFunctionValueGet
    , pinaforeLensPush
    , pinaforeNewWindow
    , PinaforeContext
    , runPinaforeAction
    , makePinaforeContext
    , nullPinaforeContext
    ) where

import Language.Expression.Dolan
import Pinafore.Base.Know
import Pinafore.Base.Morphism
import Shapes
import Truth.Core
import Truth.UI.GTK

newtype PinaforeAction baseedit a =
    MkPinaforeAction (ReaderT (WindowSpec baseedit -> IO (UIWindow, UndoActions), Object baseedit) (ComposeM Know IO) a)
    deriving (Functor, Applicative, Monad, MonadFix, MonadIO)

instance MonadFail (PinaforeAction baseedit) where
    fail s = liftIO $ fail s

instance HasDolanVary '[ 'Covariance] (PinaforeAction baseedit) where
    dolanVary = ConsDolanVarianceMap fmap $ NilDolanVarianceMap

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

pinaforeNewWindow :: WindowSpec baseedit -> PinaforeAction baseedit (UIWindow, UndoActions)
pinaforeNewWindow uiw = do
    (neww, _) <- MkPinaforeAction ask
    liftIO $ neww uiw

pinaforeActionKnow :: forall baseedit a. Know a -> PinaforeAction baseedit a
pinaforeActionKnow ka = MkPinaforeAction $ lift $ liftInner ka

knowPinaforeAction :: forall baseedit a. PinaforeAction baseedit a -> PinaforeAction baseedit (Know a)
knowPinaforeAction (MkPinaforeAction (ReaderT rka)) =
    MkPinaforeAction $ ReaderT $ \r -> MkComposeM $ fmap Known $ getComposeM $ rka r

newtype PinaforeContext baseedit =
    MkPinaforeContext (PinaforeAction baseedit () -> IO ())

runPinaforeAction :: (?pinafore :: PinaforeContext baseedit) => PinaforeAction baseedit () -> IO ()
runPinaforeAction =
    case ?pinafore of
        MkPinaforeContext unlift -> unlift

makePinaforeContext ::
       forall baseedit. InvertibleEdit baseedit
    => Object baseedit
    -> (UserInterface WindowSpec -> IO UIWindow)
    -> LifeCycle (PinaforeContext baseedit)
makePinaforeContext pinaforeObject createWindow = do
    rsub <- liftIO $ makeObjectSubscriber pinaforeObject
    (sub, uactions) <- liftIO $ undoQueueSubscriber rsub
    return $
        MkPinaforeContext $ \(MkPinaforeAction action) -> let
            openwin :: WindowSpec baseedit -> IO (UIWindow, UndoActions)
            openwin uiw = do
                window <- createWindow $ MkUserInterface sub uiw
                return (window, uactions)
            in do
                   _ <- getComposeM $ runReaderT action (openwin, subObject sub)
                   return ()

nullPinaforeContext :: PinaforeContext baseedit
nullPinaforeContext = MkPinaforeContext $ \_ -> fail "null Pinafore context"
