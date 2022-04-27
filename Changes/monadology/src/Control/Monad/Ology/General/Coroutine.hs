module Control.Monad.Ology.General.Coroutine where

import Control.Monad.Ology.General.Trans.Hoist
import Control.Monad.Ology.General.Trans.Trans
import Control.Monad.Ology.General.Trans.Tunnel
import Control.Monad.Ology.General.Trans.Unlift
import Control.Monad.Ology.Specific.CoroutineT
import Import

class Monad m => MonadCoroutine m where
    suspend :: ((p -> m q) -> m r) -> CoroutineT p q m r

coroutineSuspend ::
       (MonadCoroutine m, MonadFail m) => CoroutineT a b m (a, c) -> (a -> CoroutineT b a m (b, d)) -> m (c, d)
coroutineSuspend s1 as2 = do
    e1 <- resume s1
    case e1 of
        Left (a, c) -> do
            e2 <- resume $ as2 a
            case e2 of
                Left (_, d) -> return (c, d)
                Right _ -> fail $ "coroutine: routine terminated"
        Right (a, f) -> do
            (d, c) <- coroutineSuspend (as2 a) f
            return (c, d)

coroutine :: (MonadCoroutine m, MonadFail m) => ((a -> m b) -> m (a, c)) -> (a -> (b -> m a) -> m (b, d)) -> m (c, d)
coroutine t1 at2 = coroutineSuspend (suspend t1) $ \a -> suspend $ at2 a

instance MonadCoroutine IO where
    suspend :: ((p -> IO q) -> IO r) -> CoroutineT p q IO r
    suspend action =
        MkCoroutineT $ do
            invar <- newEmptyMVar
            outvar <- newEmptyMVar
            _ <-
                forkIO $ do
                    r <-
                        action $ \p -> do
                            putMVar outvar $
                                Right
                                    ( p
                                    , \q ->
                                          MkCoroutineT $ do
                                              putMVar invar q
                                              takeMVar outvar)
                            takeMVar invar
                    putMVar outvar $ Left r
            takeMVar outvar

instance (MonadTransUnlift t, MonadCoroutine m, MonadTunnelIOInner m, Monad (t m)) => MonadCoroutine (t m) where
    suspend call =
        MkCoroutineT $
        liftWithUnlift $ \unlift ->
            (fmap $ fmap $ fmap $ fmap $ hoist lift) $ resume $ suspend $ \pmq -> unlift $ call $ \p -> lift $ pmq p

type With m t = forall (r :: Type). (t -> m r) -> m r

unpickWith :: MonadCoroutine m => With m a -> m (a, m ())
unpickWith w = do
    etp <- resume $ suspend w
    case etp of
        Left a -> return (a, return ())
        Right (a, f) -> return (a, fmap (\_ -> ()) $ coroutineRun $ f a)
