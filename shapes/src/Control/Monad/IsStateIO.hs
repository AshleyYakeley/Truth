module Control.Monad.IsStateIO where

import Control.Monad.Trans.State.Extra
import Control.Monad.Trans.Tunnel
import Control.Monad.Trans.Unlift
import Shapes.Import

class (MonadFix m, MonadUnliftIO m) =>
      IsStateIO m where
    type IOState m :: Type
    runStateIO :: forall a. m a -> IOState m -> IO (a, IOState m)
    mkStateIO :: forall a. (IOState m -> IO (a, IOState m)) -> m a

instance IsStateIO IO where
    type IOState IO = ()
    runStateIO ioa _ = fmap (\a -> (a, ())) ioa
    mkStateIO s = fmap fst $ s ()

instance IsStateIO m => IsStateIO (StateT s m) where
    type IOState (StateT s m) = (s, IOState m)
    runStateIO (StateT smas) (s, ss) = fmap swap3' $ runStateIO (smas s) ss
    mkStateIO ssioass = StateT $ \s -> mkStateIO $ \ss -> fmap swap3 $ ssioass (s, ss)

toStateIO :: IsStateIO m => StateT (IOState m) IO a -> m a
toStateIO (StateT smas) = mkStateIO smas

fromStateIO :: IsStateIO m => m a -> StateT (IOState m) IO a
fromStateIO ma = StateT $ runStateIO ma

mapIOInvert :: (Functor f, Functor g, IsStateIO m) => (forall a. f (IO (g a)) -> IO a) -> f (m (g b)) -> m b
mapIOInvert ff fmgb =
    mkStateIO $ \oldstate ->
        ff $ fmap (\mgb -> fmap (\(gb, s) -> fmap (\b -> (b, s)) gb) $ runStateIO mgb oldstate) fmgb

tunnelStateIO :: IsStateIO m => (forall a. (m r -> IO a) -> IO a) -> m r
tunnelStateIO call = toStateIO $ tunnel $ \unlift -> call $ \mr -> unlift $ fromStateIO mr

mvarStateAccess :: IsStateIO m => MVar s -> StateAccess m s
mvarStateAccess mvar sma =
    mkStateIO $ \oldios ->
        modifyMVar mvar $ \olds -> do
            (a, (news, newios)) <- runStateIO sma (olds, oldios)
            return (news, (a, newios))

liftWithMVar :: IsStateIO m => (MVar (IOState m) -> IO r) -> m r
liftWithMVar call = do
    olds <- toStateIO get
    var <- liftIO $ newMVar olds
    r <- liftIO $ call var
    news <- liftIO $ takeMVar var
    toStateIO $ put news
    return r

ioStateAccess :: IsStateIO m => StateAccess IO (IOState m) -> UnliftIO m
ioStateAccess acc mr = acc $ fromStateIO mr

tryModifyMVar :: MVar a -> (a -> IO (a, b)) -> IO (Maybe b)
tryModifyMVar var call =
    mask $ \restore -> do
        molda <- tryTakeMVar var
        case molda of
            Just olda -> do
                (newa, b) <- restore (call olda >>= evaluate) `onException` putMVar var olda
                putMVar var newa
                return $ Just b
            Nothing -> return Nothing

mvarTryStateT :: IsStateIO m => MVar s -> StateT s m r -> m (Maybe r)
mvarTryStateT var call =
    mkStateIO $ \oldios -> do
        mrs <-
            tryModifyMVar var $ \olds -> do
                (a, (news, newios)) <- runStateIO call (olds, oldios)
                return (news, (a, newios))
        return $
            case mrs of
                Nothing -> (Nothing, oldios)
                Just (r, newios) -> (Just r, newios)

type IOStateAccess s
     = forall m. IsStateIO m =>
                     StateAccess m s

type CombineState m1 m2 = StateT (IOState m1) m2

combineStateRunners :: IsStateIO m1 => UnliftIO m1 -> UnliftIO m2 -> UnliftIO (CombineState m1 m2)
combineStateRunners r1 r2 cs = r1 $ toStateIO $ remonad r2 cs

lift1 :: (IsStateIO m1, MonadIO m2) => m1 a -> CombineState m1 m2 a
lift1 m1a = remonad liftIO $ fromStateIO m1a

lift2 :: Monad m2 => m2 a -> CombineState m1 m2 a
lift2 = lift
