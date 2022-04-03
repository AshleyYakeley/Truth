module Control.Monad.Ology.Trans.Tunnel where

import qualified Control.Exception as CE
import Control.Monad.Ology.Function
import Control.Monad.Ology.Functor.One
import Control.Monad.Ology.Trans.Constraint
import Import

type TransTunnel :: TransKind -> Constraint
class (MonadTrans t, TransConstraint Functor t, TransConstraint Monad t, FunctorOne (Tunnel t)) => TransTunnel t where
    type Tunnel t :: Type -> Type
    tunnel ::
           forall m r. Functor m
        => ((forall m1 a. Functor m1 => t m1 a -> m1 (Tunnel t a)) -> m (Tunnel t r))
        -> t m r

transExcept ::
       forall t m e a. (TransTunnel t, Monad m)
    => t (ExceptT e m) a
    -> t m (Either e a)
transExcept tema = tunnel $ \unlift -> fmap sequenceEither $ runExceptT $ unlift tema

hoist ::
       forall t m1 m2. (TransTunnel t, Functor m1, Functor m2)
    => (m1 --> m2)
    -> t m1 --> t m2
hoist mma sm1 = tunnel $ \tun -> mma $ tun sm1

hoistTransform :: (TransTunnel t, Monad m1, Monad m2) => (m1 --> m2) -> WMFunction (t m2) n -> WMFunction (t m1) n
hoistTransform ff (MkWMFunction r2) = MkWMFunction $ \m1a -> r2 $ hoist ff m1a

-- | Swap two transformers in a transformer stack
commuteTWith ::
       forall ta tb m. (TransTunnel ta, TransTunnel tb, Functor m)
    => (forall r. Tunnel tb (Tunnel ta r) -> Tunnel ta (Tunnel tb r))
    -> ta (tb m) --> tb (ta m)
commuteTWith commutef tabm =
    case hasTransConstraint @Functor @ta @m of
        Dict ->
            case hasTransConstraint @Functor @tb @m of
                Dict -> tunnel $ \unliftb -> tunnel $ \unlifta -> fmap commutef $ unliftb $ unlifta tabm

commuteT ::
       forall ta tb m. (TransTunnel ta, TransTunnel tb, FunctorExtract (Tunnel tb), Functor m)
    => ta (tb m) --> tb (ta m)
commuteT = commuteTWith fcommuteB

commuteTBack ::
       forall ta tb m.
       (TransTunnel ta, FunctorExtract (Tunnel ta), TransTunnel tb, FunctorExtract (Tunnel tb), Functor m)
    => ta (tb m) -/-> tb (ta m)
commuteTBack call = commuteT $ call commuteT

instance TransTunnel IdentityT where
    type Tunnel IdentityT = Identity
    tunnel call = IdentityT $ fmap runIdentity $ call $ \(IdentityT ma) -> fmap Identity ma

instance TransTunnel (ReaderT r) where
    type Tunnel (ReaderT r) = Identity
    tunnel call = ReaderT $ \r -> fmap runIdentity $ call $ \(ReaderT smr) -> fmap Identity $ smr r

instance Monoid w => TransTunnel (WriterT w) where
    type Tunnel (WriterT w) = (,) w
    tunnel call = WriterT $ fmap swap $ call $ \(WriterT mrs) -> fmap swap $ mrs

instance TransTunnel (StateT s) where
    type Tunnel (StateT s) = (,) (Endo s)
    tunnel call =
        StateT $ \olds ->
            fmap (\(Endo sf, r) -> (r, sf olds)) $
            call $ \(StateT smrs) -> fmap (\(a, s) -> (Endo $ pure s, a)) $ smrs olds

instance TransTunnel MaybeT where
    type Tunnel MaybeT = Maybe
    tunnel call = MaybeT $ call $ \(MaybeT ma) -> ma

instance TransTunnel (ExceptT e) where
    type Tunnel (ExceptT e) = Either e
    tunnel call = ExceptT $ call $ \(ExceptT ma) -> ma

class (MonadIO m, FunctorOne (TunnelIO m)) => MonadTunnelIO m where
    type TunnelIO m :: Type -> Type
    tunnelIO :: forall r. ((forall a. m a -> IO (TunnelIO m a)) -> IO (TunnelIO m r)) -> m r

hoistIO :: MonadTunnelIO m => (IO --> IO) -> m --> m
hoistIO mma sm1 = tunnelIO $ \tun -> mma $ tun sm1

instance MonadTunnelIO IO where
    type TunnelIO IO = Identity
    tunnelIO call = fmap runIdentity $ call $ \ma -> fmap Identity $ ma

instance (TransTunnel t, MonadTunnelIO m, MonadIO (t m)) => MonadTunnelIO (t m) where
    type TunnelIO (t m) = Compose (TunnelIO m) (Tunnel t)
    tunnelIO call =
        tunnel $ \unlift -> tunnelIO $ \unliftIO -> fmap getCompose $ call $ fmap Compose . unliftIO . unlift

instance (TransTunnel t, TransConstraint MonadIO t) => TransConstraint MonadTunnelIO t where
    hasTransConstraint = withTransConstraintDict @MonadIO $ Dict

backHoist :: (TransTunnel t, Monad ma, Monad mb) => (ma -/-> mb) -> t ma -/-> t mb
backHoist wt tm = tunnel $ \unlift -> wt $ \tba -> unlift $ tm $ hoist tba

backHoistW :: (TransTunnel t, Monad ma, Monad mb) => WMBackFunction ma mb -> WMBackFunction (t ma) (t mb)
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

identityWUnliftAll :: WUnlift c IdentityT
identityWUnliftAll = MkWUnlift runIdentityT

mVarRun :: MVar s -> Unlift MonadTunnelIO (StateT s)
mVarRun var (StateT smr) =
    tunnelIO $ \unlift ->
        modifyMVar var $ \olds ->
            fmap (\fas -> (fromMaybe olds $ fextractm $ fmap snd fas, fmap fst fas)) $ unlift $ smr olds

mVarUnitRun :: MonadTunnelIO m => MVar s -> m --> m
mVarUnitRun var ma = mVarRun var $ lift ma

mVarUnitUnlock :: MVar () -> IO --> IO
mVarUnitUnlock var = CE.bracket_ (putMVar var ()) (takeMVar var)

stateDiscardingUntrans :: s -> Unlift MonadIO (StateT s)
stateDiscardingUntrans s mr = do
    (r, _discarded) <- runStateT mr s
    return r

-- | Dangerous, because the MVar won't be released on exception.
dangerousMVarRun :: MVar s -> Unlift MonadIO (StateT s)
dangerousMVarRun var (StateT smr) = do
    olds <- liftIO $ takeMVar var
    (a, news) <- smr olds
    liftIO $ putMVar var news
    return a

liftStateT :: (Traversable f, Applicative m) => StateT s m a -> StateT (f s) m (f a)
liftStateT (StateT smas) = StateT $ \fs -> fmap (\fas -> (fmap fst fas, fmap snd fas)) $ traverse smas fs

liftWithMVarStateT :: MonadIO m => (MVar s -> m a) -> StateT s m a
liftWithMVarStateT vma =
    StateT $ \initialstate -> do
        var <- liftIO $ newMVar initialstate
        r <- vma var
        finalstate <- liftIO $ takeMVar var
        return (r, finalstate)

mVarWIORun :: MVar s -> WMFunction (StateT s IO) IO
mVarWIORun var = MkWMFunction $ mVarRun var
