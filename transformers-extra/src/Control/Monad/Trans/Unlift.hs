module Control.Monad.Trans.Unlift where

import Control.Monad.Trans.Constraint
import Control.Monad.Trans.Function
import Control.Monad.Trans.Tunnel
import Import

class ( MonadTransConstraint MonadFail t
      , MonadTransConstraint MonadIO t
      , MonadTransConstraint MonadFix t
      , MonadTransSemiTunnel t
      ) => MonadTransUnlift t where
    liftWithUnlift ::
           forall m. MonadUnliftIO m
        => MBackFunction m (t m)
    -- ^ lift with a 'WMFunction' that accounts for the transformer's effects (using MVars where necessary)
    default liftWithUnlift :: MonadTransUnliftAll t => forall m. MonadUnliftIO m => MBackFunction m (t m)
    liftWithUnlift = liftWithUnliftAll
    getDiscardingUnlift ::
           forall m. MonadUnliftIO m
        => t m (WMFunction (t m) m)
    -- ^ return a 'WMFunction' that discards the transformer's effects (such as state change or output)
    default getDiscardingUnlift :: forall m. (MonadTransUnliftAll t, MonadUnliftIO m) => t m (WMFunction (t m) m)
    getDiscardingUnlift =
        case hasTransConstraint @Monad @t @m of
            Dict -> fmap wUnliftAllWMFunction getDiscardingUnliftAll

liftWithUnliftW ::
       forall t m. (MonadTransUnlift t, MonadUnliftIO m)
    => WMBackFunction m (t m)
liftWithUnliftW = MkWMBackFunction liftWithUnlift

type UnliftAll t = forall (m :: Type -> Type). MonadUnliftIO m => MFunction (t m) m

newtype WUnliftAll (t :: (Type -> Type) -> Type -> Type) = MkWUnliftAll
    { runWUnliftAll :: UnliftAll t
    }

wUnliftAllWMFunction :: MonadUnliftIO m => WUnliftAll t -> WMFunction (t m) m
wUnliftAllWMFunction (MkWUnliftAll unlift) = MkWMFunction unlift

identityRunner :: UnliftAll IdentityT
identityRunner = runIdentityT

identityWRunner :: WUnliftAll IdentityT
identityWRunner = MkWUnliftAll runIdentityT

mVarRun :: MVar s -> UnliftAll (StateT s)
mVarRun var (StateT smr) = liftIOWithUnlift $ \unlift -> modifyMVar var $ \olds -> unlift $ fmap swap $ smr olds

wMVarRun :: MVar s -> WUnliftAll (StateT s)
wMVarRun var = MkWUnliftAll $ mVarRun var

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

readerTUnliftAllToT :: (MonadTransUnliftAll t, MonadUnliftIO m) => MFunction (ReaderT (WUnliftAll t) m) (t m)
readerTUnliftAllToT rma = liftWithUnliftAll $ \tr -> runReaderT rma $ MkWUnliftAll tr

tToReaderTUnliftAll :: MonadUnliftIO m => MFunction (t m) (ReaderT (WUnliftAll t) m)
tToReaderTUnliftAll tma = do
    MkWUnliftAll unlift <- ask
    lift $ unlift tma

class (MonadTransConstraint MonadPlus t, MonadTransTunnel t, MonadTransUnlift t) => MonadTransUnliftAll t where
    liftWithUnliftAll ::
           forall m r. MonadUnliftIO m
        => (UnliftAll t -> m r)
        -> t m r
    -- ^ lift with a 'WUnliftAll' that accounts for the transformer's effects (using MVars where necessary)
    getDiscardingUnliftAll ::
           forall m. Monad m
        => t m (WUnliftAll t)
    -- ^ return a 'WUnliftAll' that discards the transformer's effects (such as state change or output)

-- | Swap two transformers in a transformer stack
commuteT ::
       forall ta tb m. (MonadTransUnliftAll ta, MonadTransUnliftAll tb, MonadUnliftIO m)
    => MFunction (ta (tb m)) (tb (ta m))
commuteT tatbmr =
    case hasTransConstraint @MonadUnliftIO @ta @m of
        Dict -> liftWithUnliftAll $ \unlift -> remonad' unlift tatbmr

type IOFunction m = MFunction m IO

type WIOFunction m = WMFunction m IO

mVarWIORun :: MVar s -> WIOFunction (StateT s IO)
mVarWIORun var = MkWMFunction $ mVarRun var

composeUnliftAllFunction ::
       (MonadTransUnliftAll t, MonadUnliftIO m) => UnliftAll t -> MFunction m n -> MFunction (t m) n
composeUnliftAllFunction rt rm tma = rm $ rt tma

composeUnliftAllFunctionCommute ::
       (MonadTransUnliftAll t, MonadUnliftIO m, MonadUnliftIO n) => UnliftAll t -> MFunction m n -> MFunction (t m) n
composeUnliftAllFunctionCommute rt rm tma = rt $ remonad rm tma

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

instance MonadTransUnliftAll IdentityT where
    liftWithUnliftAll call = IdentityT $ call runIdentityT
    getDiscardingUnliftAll = return identityWRunner

instance MonadTransUnlift (ReaderT s)

instance MonadTransUnliftAll (ReaderT s) where
    liftWithUnliftAll call = ReaderT $ \s -> call $ \(ReaderT smr) -> smr s
    getDiscardingUnliftAll = do
        s <- ask
        return $ MkWUnliftAll $ \mr -> runReaderT mr s

instance Monoid s => MonadTransUnlift (WriterT s)

writerDiscardingUntrans :: UnliftAll (WriterT s)
writerDiscardingUntrans mr = do
    (r, _discarded) <- runWriterT mr
    return r

instance Monoid s => MonadTransUnliftAll (WriterT s) where
    liftWithUnliftAll call = do
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
    getDiscardingUnliftAll = return $ MkWUnliftAll writerDiscardingUntrans

instance MonadTransUnlift (StateT s)

stateDiscardingUntrans :: s -> UnliftAll (StateT s)
stateDiscardingUntrans s mr = do
    (r, _discarded) <- runStateT mr s
    return r

instance MonadTransUnliftAll (StateT s) where
    liftWithUnliftAll call = liftWithMVarStateT $ \var -> call $ mVarRun var
    getDiscardingUnliftAll = do
        s <- get
        return $ MkWUnliftAll $ stateDiscardingUntrans s
