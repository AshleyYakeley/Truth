module Control.Monad.Trans.Tunnel where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Constraint
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Transform
import Control.Monad.Trans.Writer
import Prelude

class (MonadTrans t, MonadTransConstraint Monad t) => MonadTransTunnel t where
    tunnel :: forall m2 r. (forall a. (forall m1. t m1 r -> m1 a) -> m2 a) -> t m2 r
    transExcept ::
           forall m e a. Monad m
        => t (ExceptT e m) a
        -> t m (Either e a)

remonad :: MonadTransTunnel t => (forall a. m1 a -> m2 a) -> t m1 r -> t m2 r
remonad mma sm1 = tunnel $ \tun -> mma $ tun sm1

remonadTransform :: MonadTransTunnel t => (forall a. m1 a -> m2 a) -> Transform (t m2) n -> Transform (t m1) n
remonadTransform ff (MkTransform r2) = MkTransform $ \m1a -> r2 $ remonad ff m1a

liftTransform :: MonadTransTunnel t => Transform m1 m2 -> Transform (t m1) (t m2)
liftTransform (MkTransform mm) = MkTransform $ remonad mm

instance MonadTransTunnel IdentityT where
    tunnel call = IdentityT $ call $ runIdentityT
    transExcept (IdentityT ma) = IdentityT $ runExceptT ma

instance MonadTransTunnel (ReaderT s) where
    tunnel call = ReaderT $ \s -> call $ \(ReaderT smr) -> smr s
    transExcept (ReaderT ma) = ReaderT $ \s -> runExceptT $ ma s

instance Monoid s => MonadTransTunnel (WriterT s) where
    tunnel call = WriterT $ call $ \(WriterT mrs) -> mrs
    transExcept (WriterT ma) =
        WriterT $
        fmap
            (\case
                 Left e -> (Left e, mempty)
                 Right (a, s) -> (Right a, s)) $
        runExceptT ma

instance MonadTransTunnel (StateT s) where
    tunnel call = StateT $ \olds -> call $ \(StateT smrs) -> smrs olds
    transExcept (StateT ma) =
        StateT $ \olds ->
            fmap
                (\case
                     Left e -> (Left e, olds)
                     Right (a, news) -> (Right a, news)) $
            runExceptT $ ma olds

instance MonadTransTunnel MaybeT where
    tunnel call = MaybeT $ call $ runMaybeT
    transExcept (MaybeT ma) =
        MaybeT $
        fmap
            (\case
                 Left e -> Just $ Left e
                 Right (Just a) -> Just $ Right a
                 Right Nothing -> Nothing) $
        runExceptT ma

instance MonadTransTunnel (ExceptT e) where
    tunnel call = ExceptT $ call $ runExceptT
    transExcept (ExceptT ma) =
        ExceptT $
        fmap
            (\case
                 Left e' -> Right $ Left e'
                 Right (Left e) -> Left e
                 Right (Right a) -> Right $ Right a) $
        runExceptT ma

class MonadIO m => MonadTunnelIO m where
    tunnelIO :: forall r. (forall a. (m r -> IO a) -> IO a) -> m r

remonadIO :: MonadTunnelIO m => (forall a. IO a -> IO a) -> m r -> m r
remonadIO mma sm1 = tunnelIO $ \tun -> mma $ tun sm1

instance MonadTunnelIO IO where
    tunnelIO call = call id

instance (MonadTransTunnel t, MonadTunnelIO m, MonadIO (t m)) => MonadTunnelIO (t m) where
    tunnelIO call = tunnel $ \tun -> tunnelIO $ \maiob -> call $ \tmr -> maiob $ tun tmr
