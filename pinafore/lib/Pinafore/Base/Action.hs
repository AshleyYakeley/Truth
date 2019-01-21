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
    MkPinaforeAction (ReaderT (UIWindow baseedit -> IO (), Object baseedit) (ComposeM Know IO) a)
    deriving (Functor, Applicative, Monad, MonadFix, MonadIO)

instance MonadFail (PinaforeAction baseedit) where
    fail s = liftIO $ fail s

instance HasDolanVary '[ 'Covariance] (PinaforeAction baseedit) where
    dolanVary = ConsDolanKindVary fmap $ NilDolanKindVary

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
       forall baseedit. Object baseedit -> (UserInterface UIWindow -> IO ()) -> LifeCycle (PinaforeContext baseedit)
makePinaforeContext pinaforeObject createWindow = do
    sub <- liftIO $ makeObjectSubscriber pinaforeObject
    (_, obj, _) <- subscribe sub (\_ -> return ()) (\_ _ _ -> return ())
    return $
        MkPinaforeContext $ \(MkPinaforeAction action) -> let
            openwin :: UIWindow baseedit -> IO ()
            openwin uiw = createWindow $ MkUserInterface sub uiw
            in do
                   _ <- getComposeM $ runReaderT action (openwin, obj)
                   return ()

nullPinaforeContext :: PinaforeContext baseedit
nullPinaforeContext = MkPinaforeContext $ \_ -> fail "null Pinafore context"
