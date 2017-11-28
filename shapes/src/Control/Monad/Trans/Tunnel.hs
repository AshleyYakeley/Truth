module Control.Monad.Trans.Tunnel where

import Control.Monad.Trans.Constraint
import Shapes.Import

class (MonadTrans t, MonadTransConstraint Monad t) =>
      MonadTransTunnel t where
    tunnel :: forall m2 r. (forall a. (forall m1. t m1 r -> m1 a) -> m2 a) -> t m2 r

remonad :: MonadTransTunnel t => (forall a. m1 a -> m2 a) -> t m1 r -> t m2 r
remonad mma sm1 = tunnel $ \tun -> mma $ tun sm1

instance MonadTransTunnel IdentityT where
    tunnel call = IdentityT $ call $ runIdentityT

instance MonadTransTunnel (ReaderT s) where
    tunnel call = ReaderT $ \s -> call $ \(ReaderT smr) -> smr s

instance Monoid s => MonadTransTunnel (WriterT s) where
    tunnel call = WriterT $ call $ \(WriterT mrs) -> mrs

instance MonadTransTunnel (StateT s) where
    tunnel call = StateT $ \olds -> call $ \(StateT smrs) -> smrs olds

instance MonadTransTunnel MaybeT where
    tunnel call = MaybeT $ call $ runMaybeT

instance MonadTransTunnel (ExceptT e) where
    tunnel call = ExceptT $ call $ runExceptT

instance MonadTransTunnel ListT where
    tunnel call = ListT $ call $ runListT

type UnliftIO m = forall r. m r -> IO r

remonadUnliftIO :: MonadTransTunnel t => (forall a. m1 a -> m2 a) -> UnliftIO (t m2) -> UnliftIO (t m1)
remonadUnliftIO ff r2 m1a = r2 $ remonad ff m1a
