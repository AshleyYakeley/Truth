module Control.Monad.Ology.General.Coroutine where

import Control.Monad.Ology.General.Trans.Hoist
import Control.Monad.Ology.General.Trans.Trans
import Control.Monad.Ology.General.Trans.Tunnel
import Control.Monad.Ology.General.Trans.Unlift
import Control.Monad.Ology.Specific.CoroutineT
import Import

class Monad m => MonadCoroutine m where
    coroutineSuspend :: ((p -> m q) -> m r) -> CoroutineT p q m r

instance MonadCoroutine IO where
    coroutineSuspend :: ((p -> IO q) -> IO r) -> CoroutineT p q IO r
    coroutineSuspend action =
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
    coroutineSuspend call =
        MkCoroutineT $
        liftWithUnlift $ \unlift ->
            (fmap $ fmap $ fmap $ fmap $ hoist lift) $
            resumeCoroutine $ coroutineSuspend $ \pmq -> unlift $ call $ \p -> lift $ pmq p

type With m t = forall (r :: Type). (t -> m r) -> m r

unpickWith ::
       forall m a. MonadCoroutine m
    => With m a
    -> m (a, m ())
unpickWith w = do
    etp <- resumeCoroutine $ coroutineSuspend w
    case etp of
        Left a -> return (a, return ())
        Right (a, f) -> return (a, fmap (\_ -> ()) $ runCoroutine $ f a)
