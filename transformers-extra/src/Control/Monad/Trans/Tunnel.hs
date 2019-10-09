module Control.Monad.Trans.Tunnel where

import Control.Category
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Constraint
import Control.Monad.Trans.Except
import Control.Monad.Trans.Function
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Prelude hiding ((.), id)

class (MonadTrans t, MonadTransConstraint Monad t) => MonadTransSemiTunnel t where
    semitunnel ::
           forall m1 m2 r. (Monad m1, Monad m2)
        => (forall a. (t m1 r -> m1 a) -> m2 a)
        -> t m2 r
    default semitunnel :: MonadTransTunnel t => forall m1 m2 r. (forall a. (t m1 r -> m1 a) -> m2 a) -> t m2 r
    semitunnel = tunnel

remonad ::
       forall t m1 m2. (MonadTransSemiTunnel t, Monad m1, Monad m2)
    => MFunction m1 m2
    -> MFunction (t m1) (t m2)
remonad mma sm1 = semitunnel $ \tun -> mma $ tun sm1

remonadTransform ::
       (MonadTransSemiTunnel t, Monad m1, Monad m2) => MFunction m1 m2 -> WMFunction (t m2) n -> WMFunction (t m1) n
remonadTransform ff (MkWMFunction r2) = MkWMFunction $ \m1a -> r2 $ remonad ff m1a

liftWMFunction :: (MonadTransSemiTunnel t, Monad m1, Monad m2) => WMFunction m1 m2 -> WMFunction (t m1) (t m2)
liftWMFunction (MkWMFunction mm) = MkWMFunction $ remonad mm

class MonadTransSemiTunnel t => MonadTransTunnel t where
    tunnel :: forall m2 r. (forall a. (forall m1. t m1 r -> m1 a) -> m2 a) -> t m2 r
    transExcept ::
           forall m e a. Monad m
        => t (ExceptT e m) a
        -> t m (Either e a)

remonad' ::
       forall t m1 m2. MonadTransTunnel t
    => MFunction m1 m2
    -> MFunction (t m1) (t m2)
remonad' mma sm1 = tunnel $ \tun -> mma $ tun sm1

liftWMFunction' :: MonadTransTunnel t => WMFunction m1 m2 -> WMFunction (t m1) (t m2)
liftWMFunction' (MkWMFunction mm) = MkWMFunction $ remonad' mm

instance MonadTransSemiTunnel IdentityT

instance MonadTransTunnel IdentityT where
    tunnel call = IdentityT $ call $ runIdentityT
    transExcept (IdentityT ma) = IdentityT $ runExceptT ma

instance MonadTransSemiTunnel (ReaderT s)

instance MonadTransTunnel (ReaderT s) where
    tunnel call = ReaderT $ \s -> call $ \(ReaderT smr) -> smr s
    transExcept (ReaderT ma) = ReaderT $ \s -> runExceptT $ ma s

instance Monoid s => MonadTransSemiTunnel (WriterT s)

instance Monoid s => MonadTransTunnel (WriterT s) where
    tunnel call = WriterT $ call $ \(WriterT mrs) -> mrs
    transExcept (WriterT ma) =
        WriterT $
        fmap
            (\case
                 Left e -> (Left e, mempty)
                 Right (a, s) -> (Right a, s)) $
        runExceptT ma

instance MonadTransSemiTunnel (StateT s)

instance MonadTransTunnel (StateT s) where
    tunnel call = StateT $ \olds -> call $ \(StateT smrs) -> smrs olds
    transExcept (StateT ma) =
        StateT $ \olds ->
            fmap
                (\case
                     Left e -> (Left e, olds)
                     Right (a, news) -> (Right a, news)) $
            runExceptT $ ma olds

instance MonadTransSemiTunnel MaybeT

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

instance MonadTransSemiTunnel (ExceptT e)

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

instance (MonadTransSemiTunnel t, MonadTunnelIO m, MonadIO (t m)) => MonadTunnelIO (t m) where
    tunnelIO call = semitunnel $ \tun -> tunnelIO $ \maiob -> call $ \tmr -> maiob $ tun tmr

liftMBackFunction :: (MonadTransSemiTunnel t, Monad ma, Monad mb) => MBackFunction ma mb -> MBackFunction (t ma) (t mb)
liftMBackFunction wt tm = semitunnel $ \unlift -> wt $ \tba -> unlift $ tm $ remonad tba

liftWMBackFunction ::
       (MonadTransSemiTunnel t, Monad ma, Monad mb) => WMBackFunction ma mb -> WMBackFunction (t ma) (t mb)
liftWMBackFunction (MkWMBackFunction f) = MkWMBackFunction $ liftMBackFunction f
