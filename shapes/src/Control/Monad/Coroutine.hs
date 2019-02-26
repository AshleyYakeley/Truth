module Control.Monad.Coroutine where

import Shapes.Import

class Monad m => MonadCoroutine m where
    coroutine :: ((a -> m b) -> m (a, c)) -> (a -> (b -> m a) -> m (b, d)) -> m (c, d)

instance MonadCoroutine IO where
    coroutine t1 t2 = do
        avar <- newEmptyMVar
        bvar <- newEmptyMVar
        dvar <- newEmptyMVar
        _ <-
            forkIO $ do
                a <- takeMVar avar
                (b, d) <-
                    t2 a $ \b -> do
                        putMVar bvar b
                        takeMVar avar
                putMVar bvar b
                putMVar dvar d
        (a, c) <-
            t1 $ \a -> do
                putMVar avar a
                takeMVar bvar
        putMVar avar a
        d <- takeMVar dvar
        return (c, d)
