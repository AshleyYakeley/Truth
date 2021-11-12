module Control.Monad.Trans.Tunnel where

import Control.Monad.Trans.Constraint
import Control.Monad.Trans.Function
import Data.Functor.One
import Import

type TransTunnel :: TransKind -> Constraint
class (MonadTrans t, TransConstraint Functor t, TransConstraint Monad t, FunctorOne (Tunnel t)) => TransTunnel t where
    type Tunnel t :: Type -> Type
    tunnel ::
           forall m2 r. Functor m2
        => ((forall m1 a. Functor m1 => t m1 a -> m1 (Tunnel t a)) -> m2 (Tunnel t r))
        -> t m2 r

transExcept ::
       forall t m e a. (TransTunnel t, Monad m)
    => t (ExceptT e m) a
    -> t m (Either e a)
transExcept tema = tunnel $ \unlift -> fmap sequenceEither $ runExceptT $ unlift tema

hoist ::
       forall t m1 m2. (TransTunnel t, Functor m1, Functor m2)
    => MFunction m1 m2
    -> MFunction (t m1) (t m2)
hoist mma sm1 = tunnel $ \tun -> mma $ tun sm1

hoistTransform :: (TransTunnel t, Monad m1, Monad m2) => MFunction m1 m2 -> WMFunction (t m2) n -> WMFunction (t m1) n
hoistTransform ff (MkWMFunction r2) = MkWMFunction $ \m1a -> r2 $ hoist ff m1a

-- | Swap two transformers in a transformer stack
commuteTWith ::
       forall ta tb m. (TransTunnel ta, TransTunnel tb, Functor m)
    => (forall r. Tunnel tb (Tunnel ta r) -> Tunnel ta (Tunnel tb r))
    -> MFunction (ta (tb m)) (tb (ta m))
commuteTWith commutef tabm =
    case hasTransConstraint @Functor @ta @m of
        Dict ->
            case hasTransConstraint @Functor @tb @m of
                Dict -> tunnel $ \unliftb -> tunnel $ \unlifta -> fmap commutef $ unliftb $ unlifta tabm

commuteT ::
       forall ta tb m. (TransTunnel ta, TransTunnel tb, FunctorExtract (Tunnel tb), Functor m)
    => MFunction (ta (tb m)) (tb (ta m))
commuteT = commuteTWith commuteOne

commuteTBack ::
       forall ta tb m.
       (TransTunnel ta, FunctorExtract (Tunnel ta), TransTunnel tb, FunctorExtract (Tunnel tb), Functor m)
    => MBackFunction (ta (tb m)) (tb (ta m))
commuteTBack call = commuteT $ call commuteT

instance TransTunnel IdentityT where
    type Tunnel IdentityT = Identity
    tunnel call = IdentityT $ fmap runIdentity $ call $ \(IdentityT ma) -> fmap Identity ma

instance TransTunnel (ReaderT s) where
    type Tunnel (ReaderT s) = Identity
    tunnel call = ReaderT $ \s -> fmap runIdentity $ call $ \(ReaderT smr) -> fmap Identity $ smr s

instance Monoid s => TransTunnel (WriterT s) where
    type Tunnel (WriterT s) = (,) s
    tunnel call = WriterT $ fmap swap $ call $ \(WriterT mrs) -> fmap swap $ mrs

instance TransTunnel (StateT s) where
    type Tunnel (StateT s) = MaybePair s
    tunnel call =
        StateT $ \olds ->
            fmap (\(MkMaybePair ms r) -> (r, fromMaybe olds ms)) $
            call $ \(StateT smrs) -> fmap (\(a, s) -> MkMaybePair (Just s) a) $ smrs olds

instance TransTunnel MaybeT where
    type Tunnel MaybeT = Maybe
    tunnel call = MaybeT $ call $ \(MaybeT ma) -> ma

instance TransTunnel (ExceptT e) where
    type Tunnel (ExceptT e) = Either e
    tunnel call = ExceptT $ call $ \(ExceptT ma) -> ma

class (MonadIO m, FunctorOne (TunnelIO m)) => MonadTunnelIO m where
    type TunnelIO m :: Type -> Type
    tunnelIO :: forall r. ((forall a. m a -> IO (TunnelIO m a)) -> IO (TunnelIO m r)) -> m r

hoistIO :: MonadTunnelIO m => (forall a. IO a -> IO a) -> m r -> m r
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

liftMBackFunction :: (TransTunnel t, Monad ma, Monad mb) => MBackFunction ma mb -> MBackFunction (t ma) (t mb)
liftMBackFunction wt tm = tunnel $ \unlift -> wt $ \tba -> unlift $ tm $ hoist tba

liftWMBackFunction :: (TransTunnel t, Monad ma, Monad mb) => WMBackFunction ma mb -> WMBackFunction (t ma) (t mb)
liftWMBackFunction (MkWMBackFunction f) = MkWMBackFunction $ liftMBackFunction f

---
type UnliftT :: ((Type -> Type) -> Constraint) -> TransKind -> Type
type UnliftT c t = forall (m :: Type -> Type). c m => MFunction (t m) m

type WUnliftT :: ((Type -> Type) -> Constraint) -> TransKind -> Type
newtype WUnliftT c t = MkWUnliftT
    { runWUnliftT :: UnliftT c t
    }

wUnliftAllWMFunction :: c m => WUnliftT c t -> WMFunction (t m) m
wUnliftAllWMFunction (MkWUnliftT unlift) = MkWMFunction unlift

identityWUnliftAll :: WUnliftT c IdentityT
identityWUnliftAll = MkWUnliftT runIdentityT

mVarRun :: MVar s -> UnliftT MonadTunnelIO (StateT s)
mVarRun var (StateT smr) =
    tunnelIO $ \unlift ->
        modifyMVar var $ \olds ->
            fmap (\fas -> (fromMaybe olds $ getMaybeOne $ fmap snd fas, fmap fst fas)) $ unlift $ smr olds

mVarUnitRun :: MonadTunnelIO m => MVar s -> MFunction m m
mVarUnitRun var ma = mVarRun var $ lift ma

mVarUnitUnlock :: MVar () -> MFunction IO IO
mVarUnitUnlock var = bracket_ (putMVar var ()) (takeMVar var)

stateDiscardingUntrans :: s -> UnliftT MonadIO (StateT s)
stateDiscardingUntrans s mr = do
    (r, _discarded) <- runStateT mr s
    return r

-- | Dangerous, because the MVar won't be released on exception.
dangerousMVarRun :: MVar s -> UnliftT MonadIO (StateT s)
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

type IOFunction m = MFunction m IO

type WIOFunction m = WMFunction m IO

mVarWIORun :: MVar s -> WIOFunction (StateT s IO)
mVarWIORun var = MkWMFunction $ mVarRun var
