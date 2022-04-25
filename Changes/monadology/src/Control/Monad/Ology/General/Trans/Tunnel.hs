module Control.Monad.Ology.General.Trans.Tunnel where

import Control.Monad.Ology.General.Function
import Control.Monad.Ology.General.IO
import Control.Monad.Ology.General.Inner
import Control.Monad.Ology.General.Trans.Constraint
import Control.Monad.Ology.General.Trans.Trans
import Control.Monad.Ology.Specific.ComposeInner
import Import

type MonadTransTunnel :: TransKind -> Constraint
class (MonadTrans t, TransConstraint Functor t, TransConstraint Monad t, Functor (Tunnel t)) => MonadTransTunnel t where
    type Tunnel t :: Type -> Type
    tunnel ::
           forall m r. Monad m
        => ((forall m1 a. Monad m1 => t m1 a -> m1 (Tunnel t a)) -> m (Tunnel t r))
        -> t m r

hoist ::
       forall t m1 m2. (MonadTransTunnel t, Monad m1, Monad m2)
    => (m1 --> m2)
    -> t m1 --> t m2
hoist mma sm1 = tunnel $ \tun -> mma $ tun sm1

hoistTransform :: (MonadTransTunnel t, Monad m1, Monad m2) => (m1 --> m2) -> WMFunction (t m2) n -> WMFunction (t m1) n
hoistTransform ff (MkWMFunction r2) = MkWMFunction $ \m1a -> r2 $ hoist ff m1a

-- | Swap two transformers in a transformer stack
commuteTWith ::
       forall ta tb m. (MonadTransTunnel ta, MonadTransTunnel tb, Monad m)
    => (forall r. Tunnel tb (Tunnel ta r) -> Tunnel ta (Tunnel tb r))
    -> ta (tb m) --> tb (ta m)
commuteTWith commutef tabm =
    case hasTransConstraint @Monad @ta @m of
        Dict ->
            case hasTransConstraint @Monad @tb @m of
                Dict -> tunnel $ \unliftb -> tunnel $ \unlifta -> fmap commutef $ unliftb $ unlifta tabm

commuteT ::
       forall ta tb m. (MonadTransTunnel ta, MonadTransTunnel tb, Monad (Tunnel ta), MonadInner (Tunnel tb), Monad m)
    => ta (tb m) --> tb (ta m)
commuteT = commuteTWith commuteInner

commuteTBack ::
       forall ta tb m.
       (MonadTransTunnel ta, MonadTransTunnel tb, MonadInner (Tunnel ta), MonadInner (Tunnel tb), Monad m)
    => ta (tb m) -/-> tb (ta m)
commuteTBack call = commuteT $ call commuteT

instance MonadInner inner => MonadTransTunnel (ComposeInner inner) where
    type Tunnel (ComposeInner inner) = inner
    tunnel call = MkComposeInner $ call getComposeInner

class (MonadIO m, MonadInner (TunnelIO m)) => MonadTunnelIO m where
    type TunnelIO m :: Type -> Type
    tunnelIO :: forall r. ((forall a. m a -> IO (TunnelIO m a)) -> IO (TunnelIO m r)) -> m r

hoistIO :: MonadTunnelIO m => (IO --> IO) -> m --> m
hoistIO mma sm1 = tunnelIO $ \tun -> mma $ tun sm1

instance MonadTunnelIO IO where
    type TunnelIO IO = Identity
    tunnelIO call = fmap runIdentity $ call $ \ma -> fmap Identity $ ma

instance (MonadTransTunnel t, MonadInner (Tunnel t), MonadTunnelIO m, MonadIO (t m)) => MonadTunnelIO (t m) where
    type TunnelIO (t m) = ComposeInner (Tunnel t) (TunnelIO m)
    tunnelIO call =
        tunnel $ \unlift ->
            tunnelIO $ \unliftIO -> fmap getComposeInner $ call $ fmap MkComposeInner . unliftIO . unlift

instance (MonadTransTunnel t, MonadInner (Tunnel t), TransConstraint MonadIO t) => TransConstraint MonadTunnelIO t where
    hasTransConstraint = withTransConstraintDict @MonadIO $ Dict

backHoist :: (MonadTransTunnel t, Monad ma, Monad mb) => (ma -/-> mb) -> t ma -/-> t mb
backHoist wt tm = tunnel $ \unlift -> wt $ \tba -> unlift $ tm $ hoist tba

backHoistW :: (MonadTransTunnel t, Monad ma, Monad mb) => WMBackFunction ma mb -> WMBackFunction (t ma) (t mb)
backHoistW (MkWMBackFunction f) = MkWMBackFunction $ backHoist f

---
type Unlift :: ((Type -> Type) -> Constraint) -> TransKind -> Type
type Unlift c t = forall (m :: Type -> Type). c m => t m --> m

type WUnlift :: ((Type -> Type) -> Constraint) -> TransKind -> Type
newtype WUnlift c t = MkWUnlift
    { runWUnlift :: Unlift c t
    }

wUnliftAllWMFunction :: c m => WUnlift c t -> WMFunction (t m) m
wUnliftAllWMFunction (MkWUnlift unlift) = MkWMFunction unlift
