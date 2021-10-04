module Control.Monad.Trans.Tunnel where

import Control.Monad.Trans.Constraint
import Control.Monad.Trans.Function
import Data.Functor.One
import Import

-- not sure if this class is necessary tbh.
type MonadTransSemiTunnel :: TransKind -> Constraint
class (MonadTrans t, TransConstraint Functor t, TransConstraint Monad t) => MonadTransSemiTunnel t where
    semitunnel ::
           forall m1 m2 r. (Monad m1, Monad m2)
        => (forall a. (t m1 r -> m1 a) -> m2 a)
        -> t m2 r
    default semitunnel ::
        MonadTransTunnel t => forall m1 m2 r. (Functor m1, Functor m2) => (forall a. (t m1 r -> m1 a) -> m2 a) -> t m2 r
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

type MonadTransTunnel :: TransKind -> Constraint
class (MonadTransSemiTunnel t) => MonadTransTunnel t where
    transExcept ::
           forall m e a. Monad m
        => t (ExceptT e m) a
        -> t m (Either e a)
    tunnel ::
           forall m2 r. Functor m2
        => (forall f. FunctorOne f => (forall m1 a. Functor m1 => t m1 a -> m1 (f a)) -> m2 (f r))
        -> t m2 r

remonad' ::
       forall t m1 m2. (MonadTransTunnel t, Functor m1, Functor m2)
    => MFunction m1 m2
    -> MFunction (t m1) (t m2)
remonad' mma sm1 = tunnel $ \tun -> mma $ tun sm1

liftWMFunction' :: (MonadTransTunnel t, Functor m1, Functor m2) => WMFunction m1 m2 -> WMFunction (t m1) (t m2)
liftWMFunction' (MkWMFunction mm) = MkWMFunction $ remonad' mm

instance MonadTransSemiTunnel IdentityT

instance MonadTransTunnel IdentityT where
    transExcept (IdentityT ma) = IdentityT $ runExceptT ma
    tunnel call = IdentityT $ fmap runIdentity $ call $ \(IdentityT ma) -> fmap Identity ma

instance MonadTransSemiTunnel (ReaderT s)

instance MonadTransTunnel (ReaderT s) where
    transExcept (ReaderT ma) = ReaderT $ \s -> runExceptT $ ma s
    tunnel call = ReaderT $ \s -> fmap runIdentity $ call $ \(ReaderT smr) -> fmap Identity $ smr s

instance Monoid s => MonadTransSemiTunnel (WriterT s)

instance Monoid s => MonadTransTunnel (WriterT s) where
    transExcept (WriterT ma) =
        WriterT $
        fmap
            (\case
                 Left e -> (Left e, mempty)
                 Right (a, s) -> (Right a, s)) $
        runExceptT ma
    tunnel call = WriterT $ fmap swap $ call $ \(WriterT mrs) -> fmap swap $ mrs

instance MonadTransSemiTunnel (StateT s)

instance MonadTransTunnel (StateT s) where
    transExcept (StateT ma) =
        StateT $ \olds ->
            fmap
                (\case
                     Left e -> (Left e, olds)
                     Right (a, news) -> (Right a, news)) $
            runExceptT $ ma olds
    tunnel call = StateT $ \olds -> fmap swap $ call $ \(StateT smrs) -> fmap swap $ smrs olds

instance MonadTransSemiTunnel MaybeT

instance MonadTransTunnel MaybeT where
    transExcept (MaybeT ma) =
        MaybeT $
        fmap
            (\case
                 Left e -> Just $ Left e
                 Right (Just a) -> Just $ Right a
                 Right Nothing -> Nothing) $
        runExceptT ma
    tunnel call = MaybeT $ call $ \(MaybeT ma) -> ma

instance MonadTransSemiTunnel (ExceptT e)

instance MonadTransTunnel (ExceptT e) where
    transExcept (ExceptT ma) =
        ExceptT $
        fmap
            (\case
                 Left e' -> Right $ Left e'
                 Right (Left e) -> Left e
                 Right (Right a) -> Right $ Right a) $
        runExceptT ma
    tunnel call = ExceptT $ call $ \(ExceptT ma) -> ma

class MonadIO m => MonadTunnelIO m where
    tunnelIO :: forall r. (forall f. FunctorOne f => (forall a. m a -> IO (f a)) -> IO (f r)) -> m r

remonadIO :: MonadTunnelIO m => (forall a. IO a -> IO a) -> m r -> m r
remonadIO mma sm1 = tunnelIO $ \tun -> mma $ tun sm1

instance MonadTunnelIO IO where
    tunnelIO call = fmap runIdentity $ call $ \ma -> fmap Identity $ ma

instance (MonadTransTunnel t, MonadTunnelIO m, MonadIO (t m)) => MonadTunnelIO (t m) where
    tunnelIO call =
        tunnel $ \unlift -> tunnelIO $ \unliftIO -> fmap getCompose $ call $ fmap Compose . unliftIO . unlift

instance (MonadTransTunnel t, TransConstraint MonadIO t) => TransConstraint MonadTunnelIO t where
    hasTransConstraint = withTransConstraintDict @MonadIO $ Dict

liftMBackFunction :: (MonadTransSemiTunnel t, Monad ma, Monad mb) => MBackFunction ma mb -> MBackFunction (t ma) (t mb)
liftMBackFunction wt tm = semitunnel $ \unlift -> wt $ \tba -> unlift $ tm $ remonad tba

liftWMBackFunction ::
       (MonadTransSemiTunnel t, Monad ma, Monad mb) => WMBackFunction ma mb -> WMBackFunction (t ma) (t mb)
liftWMBackFunction (MkWMBackFunction f) = MkWMBackFunction $ liftMBackFunction f
