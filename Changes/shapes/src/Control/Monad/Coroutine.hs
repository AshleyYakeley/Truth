module Control.Monad.Coroutine
    ( Suspended(..)
    , runSuspendedUntilDone
    , MonadCoroutine(..)
    , coroutine
    ) where

import Shapes.Import

newtype Suspended p q m a = MkSuspended
    { resume :: m (Either a (p, q -> Suspended p q m a))
    }

runSuspendedUntilDone :: Monad m => Suspended p p m a -> m a
runSuspendedUntilDone susp = do
    eap <- resume susp
    case eap of
        Left a -> return a
        Right (p, psusp) -> runSuspendedUntilDone $ psusp p

class Monad m => MonadCoroutine m where
    suspend :: ((p -> m q) -> m r) -> Suspended p q m r

coroutineSuspend ::
       (MonadCoroutine m, MonadFail m) => Suspended a b m (a, c) -> (a -> Suspended b a m (b, d)) -> m (c, d)
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
    suspend :: ((p -> IO q) -> IO r) -> Suspended p q IO r
    suspend action =
        MkSuspended $ do
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
                                          MkSuspended $ do
                                              putMVar invar q
                                              takeMVar outvar)
                            takeMVar invar
                    putMVar outvar $ Left r
            takeMVar outvar

liftSuspended :: (MonadTrans t, Monad m) => Suspended p q m a -> Suspended p q (t m) a
liftSuspended (MkSuspended res) = MkSuspended $ lift $ fmap liftResume res

liftResume ::
       (MonadTrans t, Monad m) => Either a (p, q -> Suspended p q m a) -> Either a (p, q -> Suspended p q (t m) a)
liftResume = fmap $ fmap $ fmap liftSuspended

instance (MonadTransUnlift t, MonadCoroutine m, MonadTunnelIO m, Monad (t m)) => MonadCoroutine (t m) where
    suspend call =
        MkSuspended $
        liftWithUnlift $ \unlift -> fmap liftResume $ resume $ suspend $ \pmq -> unlift $ call $ \p -> lift $ pmq p
