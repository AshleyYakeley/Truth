module Control.Monad.Trans.Unlift where

import Control.Category
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Constraint
import Control.Monad.Trans.Function
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Tunnel
import Control.Monad.Trans.Writer
import Data.Constraint
import Data.Kind
import Data.Tuple
import Prelude hiding ((.), id)

class ( MonadTransConstraint MonadFail t
      , MonadTransConstraint MonadIO t
      , MonadTransConstraint MonadFix t
      , MonadTransSemiTunnel t
      ) => MonadTransUnlift t where
    liftWithUnlift ::
           forall m. MonadUnliftIO m
        => MBackFunction m (t m)
    -- ^ lift with a 'WMFunction' that accounts for the transformer's effects (using MVars where necessary)
    default liftWithUnlift :: MonadTransUntrans t => forall m. MonadUnliftIO m => MBackFunction m (t m)
    liftWithUnlift = liftWithUntrans
    getDiscardingUnlift ::
           forall m. MonadUnliftIO m
        => t m (WMFunction (t m) m)
    -- ^ return a 'WMFunction' that discards the transformer's effects (such as state change or output)
    default getDiscardingUnlift :: forall m. (MonadTransUntrans t, MonadUnliftIO m) => t m (WMFunction (t m) m)
    getDiscardingUnlift =
        case hasTransConstraint @Monad @t @m of
            Dict -> fmap wUntransWMFunction getDiscardingUntrans

liftWithUnliftW ::
       forall t m. (MonadTransUnlift t, MonadUnliftIO m)
    => WMBackFunction m (t m)
liftWithUnliftW = MkWMBackFunction liftWithUnlift

type Untrans t = forall (m :: Type -> Type). MonadUnliftIO m => MFunction (t m) m

newtype WUntrans (t :: (Type -> Type) -> Type -> Type) = MkWUntrans
    { runWUntrans :: Untrans t
    }

wUntransWMFunction :: MonadUnliftIO m => WUntrans t -> WMFunction (t m) m
wUntransWMFunction (MkWUntrans unlift) = MkWMFunction unlift

identityUntrans :: Untrans IdentityT
identityUntrans = runIdentityT

identityWUntrans :: WUntrans IdentityT
identityWUntrans = MkWUntrans runIdentityT

mVarRun :: MVar s -> Untrans (StateT s)
mVarRun var (StateT smr) = liftIOWithUnlift $ \unlift -> modifyMVar var $ \olds -> unlift $ fmap swap $ smr olds

wMVarRun :: MVar s -> WUntrans (StateT s)
wMVarRun var = MkWUntrans $ mVarRun var

liftStateT :: (Traversable f, Applicative m) => StateT s m a -> StateT (f s) m (f a)
liftStateT (StateT smas) = StateT $ \fs -> fmap (\fas -> (fmap fst fas, fmap snd fas)) $ traverse smas fs

liftWithMVarStateT :: MonadIO m => (MVar s -> m a) -> StateT s m a
liftWithMVarStateT vma = do
    initialstate <- get
    var <- liftIO $ newMVar initialstate
    r <- lift $ vma var
    finalstate <- liftIO $ takeMVar var
    put finalstate
    return r

readerTUnliftToT :: (MonadTransUntrans t, MonadUnliftIO m) => MFunction (ReaderT (WUntrans t) m) (t m)
readerTUnliftToT rma = liftWithUntrans $ \tr -> runReaderT rma $ MkWUntrans tr

tToReaderTUnlift :: MonadUnliftIO m => MFunction (t m) (ReaderT (WUntrans t) m)
tToReaderTUnlift tma = do
    MkWUntrans unlift <- ask
    lift $ unlift tma

class (MonadTransConstraint MonadPlus t, MonadTransTunnel t, MonadTransUnlift t) => MonadTransUntrans t where
    liftWithUntrans ::
           forall m r. MonadUnliftIO m
        => (Untrans t -> m r)
        -> t m r
    -- ^ lift with an 'WUntrans' that accounts for the transformer's effects (using MVars where necessary)
    getDiscardingUntrans ::
           forall m. Monad m
        => t m (WUntrans t)
    -- ^ return an 'WUntrans' that discards the transformer's effects (such as state change or output)

-- | Swap two transformers in a transformer stack
commuteT ::
       forall ta tb m. (MonadTransUntrans ta, MonadTransUntrans tb, MonadUnliftIO m)
    => MFunction (ta (tb m)) (tb (ta m))
commuteT tatbmr =
    case hasTransConstraint @MonadUnliftIO @ta @m of
        Dict -> liftWithUntrans $ \unlift -> remonad' unlift tatbmr

type IOFunction m = MFunction m IO

type WIOFunction m = WMFunction m IO

mVarWIORun :: MVar s -> WIOFunction (StateT s IO)
mVarWIORun var = MkWMFunction $ mVarRun var

composeUntransFunction :: (MonadTransUntrans t, MonadUnliftIO m) => Untrans t -> MFunction m n -> MFunction (t m) n
composeUntransFunction rt rm tma = rm $ rt tma

composeUntransFunctionCommute ::
       (MonadTransUntrans t, MonadUnliftIO m, MonadUnliftIO n) => Untrans t -> MFunction m n -> MFunction (t m) n
composeUntransFunctionCommute rt rm tma = rt $ remonad rm tma

class (MonadFail m, MonadTunnelIO m, MonadFix m) => MonadUnliftIO m where
    liftIOWithUnlift :: forall r. (MFunction m IO -> IO r) -> m r
    -- ^ lift with an 'WIOFunction' that accounts for all transformer effects
    getDiscardingIOUnlift :: m (WIOFunction m)
    -- ^ return an 'WIOFunction' that discards all transformer effects (such as state change or output)

ioWMBackFunction :: MonadUnliftIO m => WMBackFunction IO m
ioWMBackFunction = MkWMBackFunction liftIOWithUnlift

instance MonadUnliftIO IO where
    liftIOWithUnlift call = call id
    getDiscardingIOUnlift = return $ MkWMFunction id

instance (MonadTransUnlift t, MonadUnliftIO m, MonadFail (t m), MonadIO (t m), MonadFix (t m)) => MonadUnliftIO (t m) where
    liftIOWithUnlift call = liftWithUnlift $ \tmama -> liftIOWithUnlift $ \maioa -> call $ maioa . tmama
    getDiscardingIOUnlift = do
        MkWMFunction unlift <- getDiscardingUnlift
        MkWMFunction unliftIO <- lift getDiscardingIOUnlift
        return $ MkWMFunction $ unliftIO . unlift

instance MonadTransUnlift t => MonadTransConstraint MonadUnliftIO t where
    hasTransConstraint =
        withTransConstraintDict @MonadFail $ withTransConstraintDict @MonadIO $ withTransConstraintDict @MonadFix $ Dict

instance MonadTransUnlift IdentityT

instance MonadTransUntrans IdentityT where
    liftWithUntrans call = IdentityT $ call runIdentityT
    getDiscardingUntrans = return identityWUntrans

instance MonadTransUnlift (ReaderT s)

instance MonadTransUntrans (ReaderT s) where
    liftWithUntrans call = ReaderT $ \s -> call $ \(ReaderT smr) -> smr s
    getDiscardingUntrans = do
        s <- ask
        return $ MkWUntrans $ \mr -> runReaderT mr s

instance Monoid s => MonadTransUnlift (WriterT s)

writerDiscardingUntrans :: Untrans (WriterT s)
writerDiscardingUntrans mr = do
    (r, _discarded) <- runWriterT mr
    return r

instance Monoid s => MonadTransUntrans (WriterT s) where
    liftWithUntrans call = do
        var <- liftIO $ newMVar mempty
        r <-
            lift $
            call $ \(WriterT mrs) -> do
                (r, output) <- mrs
                liftIO $ modifyMVar var $ \oldoutput -> return (mappend oldoutput output, ())
                return r
        totaloutput <- liftIO $ takeMVar var
        tell totaloutput
        return r
    getDiscardingUntrans = return $ MkWUntrans writerDiscardingUntrans

instance MonadTransUnlift (StateT s)

stateDiscardingUntrans :: s -> Untrans (StateT s)
stateDiscardingUntrans s mr = do
    (r, _discarded) <- runStateT mr s
    return r

instance MonadTransUntrans (StateT s) where
    liftWithUntrans call = liftWithMVarStateT $ \var -> call $ mVarRun var
    getDiscardingUntrans = do
        s <- get
        return $ MkWUntrans $ stateDiscardingUntrans s
