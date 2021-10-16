module Control.Monad.Trans.Tunnel where

import Control.Monad.Trans.Constraint
import Control.Monad.Trans.Function
import Data.Functor.One
import Import

type MonadTransTunnel :: TransKind -> Constraint
class (MonadTrans t, TransConstraint Functor t, TransConstraint Monad t) => MonadTransTunnel t where
    tunnel ::
           forall m2 r. Functor m2
        => (forall f. FunctorOne f => (forall m1 a. Functor m1 => t m1 a -> m1 (f a)) -> m2 (f r))
        -> t m2 r

transExcept ::
       forall t m e a. (MonadTransTunnel t, Monad m)
    => t (ExceptT e m) a
    -> t m (Either e a)
transExcept tema = tunnel $ \unlift -> fmap sequenceEither $ runExceptT $ unlift tema

hoist ::
       forall t m1 m2. (MonadTransTunnel t, Functor m1, Functor m2)
    => MFunction m1 m2
    -> MFunction (t m1) (t m2)
hoist mma sm1 = tunnel $ \tun -> mma $ tun sm1

hoistTransform ::
       (MonadTransTunnel t, Monad m1, Monad m2) => MFunction m1 m2 -> WMFunction (t m2) n -> WMFunction (t m1) n
hoistTransform ff (MkWMFunction r2) = MkWMFunction $ \m1a -> r2 $ hoist ff m1a

instance MonadTransTunnel IdentityT where
    tunnel call = IdentityT $ fmap runIdentity $ call $ \(IdentityT ma) -> fmap Identity ma

instance MonadTransTunnel (ReaderT s) where
    tunnel call = ReaderT $ \s -> fmap runIdentity $ call $ \(ReaderT smr) -> fmap Identity $ smr s

instance Monoid s => MonadTransTunnel (WriterT s) where
    tunnel call = WriterT $ fmap swap $ call $ \(WriterT mrs) -> fmap swap $ mrs

instance MonadTransTunnel (StateT s) where
    tunnel call =
        StateT $ \olds ->
            fmap (\(MkMaybePair ms r) -> (r, fromMaybe olds ms)) $
            call $ \(StateT smrs) -> fmap (\(a, s) -> MkMaybePair (Just s) a) $ smrs olds

instance MonadTransTunnel MaybeT where
    tunnel call = MaybeT $ call $ \(MaybeT ma) -> ma

instance MonadTransTunnel (ExceptT e) where
    tunnel call = ExceptT $ call $ \(ExceptT ma) -> ma

class MonadIO m => MonadTunnelIO m where
    tunnelIO :: forall r. (forall f. FunctorOne f => (forall a. m a -> IO (f a)) -> IO (f r)) -> m r

hoistIO :: MonadTunnelIO m => (forall a. IO a -> IO a) -> m r -> m r
hoistIO mma sm1 = tunnelIO $ \tun -> mma $ tun sm1

instance MonadTunnelIO IO where
    tunnelIO call = fmap runIdentity $ call $ \ma -> fmap Identity $ ma

instance (MonadTransTunnel t, MonadTunnelIO m, MonadIO (t m)) => MonadTunnelIO (t m) where
    tunnelIO call =
        tunnel $ \unlift -> tunnelIO $ \unliftIO -> fmap getCompose $ call $ fmap Compose . unliftIO . unlift

instance (MonadTransTunnel t, TransConstraint MonadIO t) => TransConstraint MonadTunnelIO t where
    hasTransConstraint = withTransConstraintDict @MonadIO $ Dict

liftMBackFunction :: (MonadTransTunnel t, Monad ma, Monad mb) => MBackFunction ma mb -> MBackFunction (t ma) (t mb)
liftMBackFunction wt tm = tunnel $ \unlift -> wt $ \tba -> unlift $ tm $ hoist tba

liftWMBackFunction :: (MonadTransTunnel t, Monad ma, Monad mb) => WMBackFunction ma mb -> WMBackFunction (t ma) (t mb)
liftWMBackFunction (MkWMBackFunction f) = MkWMBackFunction $ liftMBackFunction f
