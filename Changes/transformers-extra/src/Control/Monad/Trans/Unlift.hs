module Control.Monad.Trans.Unlift where

import Control.Monad.Trans.Constraint
import Control.Monad.Trans.Function
import Control.Monad.Trans.Tunnel
import Data.Functor.One
import Import

class ( MonadTransTunnel t
      , TransConstraint MonadFail t
      , TransConstraint MonadIO t
      , TransConstraint MonadFix t
      , TransConstraint Monad t
      ) => MonadTransUnlift t where
    liftWithUnlift ::
           forall m. MonadTunnelIO m
        => MBackFunction m (t m)
    -- ^ lift with a 'WMFunction' that accounts for the transformer's effects (using MVars where necessary)
    default liftWithUnlift :: MonadTransUnliftAll t => forall m. MonadTunnelIO m => MBackFunction m (t m)
    liftWithUnlift = liftWithUnliftAll
    getDiscardingUnlift ::
           forall m. MonadTunnelIO m
        => t m (WMFunction (t m) m)
    -- ^ return a 'WMFunction' that discards the transformer's effects (such as state change or output)
    default getDiscardingUnlift :: forall m. (MonadTransUnliftAll t, MonadTunnelIO m) => t m (WMFunction (t m) m)
    getDiscardingUnlift =
        case hasTransConstraint @Monad @t @m of
            Dict -> fmap wUnliftAllWMFunction getDiscardingUnliftAll

liftWithUnliftW ::
       forall t m. (MonadTransUnlift t, MonadTunnelIO m)
    => WMBackFunction m (t m)
liftWithUnliftW = MkWMBackFunction liftWithUnlift

type UnliftAll c (t :: TransKind) = forall (m :: Type -> Type). c m => MFunction (t m) m

newtype WUnliftAll c (t :: TransKind) = MkWUnliftAll
    { runWUnliftAll :: UnliftAll c t
    }

wUnliftAllWMFunction :: c m => WUnliftAll c t -> WMFunction (t m) m
wUnliftAllWMFunction (MkWUnliftAll unlift) = MkWMFunction unlift

identityWUnliftAll :: WUnliftAll c IdentityT
identityWUnliftAll = MkWUnliftAll runIdentityT

mVarRun :: MVar s -> UnliftAll MonadTunnelIO (StateT s)
mVarRun var (StateT smr) =
    tunnelIO $ \unlift ->
        modifyMVar var $ \olds ->
            fmap (\fas -> (fromMaybe olds $ getMaybeOne $ fmap snd fas, fmap fst fas)) $ unlift $ smr olds

mVarUnitRun :: MonadTunnelIO m => MVar s -> MFunction m m
mVarUnitRun var ma = mVarRun var $ lift ma

mVarUnitUnlock :: MVar () -> MFunction IO IO
mVarUnitUnlock var = bracket_ (putMVar var ()) (takeMVar var)

-- | Dangerous, because the MVar won't be released on exception.
dangerousMVarRun :: MVar s -> UnliftAll MonadIO (StateT s)
dangerousMVarRun var (StateT smr) = do
    olds <- liftIO $ takeMVar var
    (a, news) <- smr olds
    liftIO $ putMVar var news
    return a

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

readerTUnliftAllToT ::
       (MonadTransUnliftAll t, MonadTunnelIO m) => MFunction (ReaderT (WUnliftAll MonadTunnelIO t) m) (t m)
readerTUnliftAllToT rma = liftWithUnliftAll $ \tr -> runReaderT rma $ MkWUnliftAll tr

tToReaderTUnliftAll :: MonadTunnelIO m => MFunction (t m) (ReaderT (WUnliftAll MonadTunnelIO t) m)
tToReaderTUnliftAll tma = do
    MkWUnliftAll unlift <- ask
    lift $ unlift tma

class (TransConstraint MonadPlus t, MonadTransTunnel t, MonadTransUnlift t) => MonadTransUnliftAll t where
    insideOut ::
           forall m r. Monad m
        => (forall b. (forall mm a. Monad mm => t mm a -> mm (a, b)) -> m (r, b))
        -> t m r
    liftWithUnliftAll ::
           forall m r. MonadIO m
        => (UnliftAll MonadTunnelIO t -> m r)
        -> t m r
    -- ^ lift with a 'WUnliftAll Monad that accounts for the transformer's effects (using MVars where necessary)
    getDiscardingUnliftAll ::
           forall m. Monad m
        => t m (WUnliftAll MonadTunnelIO t)
    -- ^ return a 'WUnliftAll Monad that discards the transformer's effects (such as state change or output)

discardingRunner ::
       forall t. MonadTransUnliftAll t
    => UnliftAll MonadUnliftIO t
    -> UnliftAll MonadUnliftIO t
discardingRunner run tmr = do
    MkWUnliftAll du <- run getDiscardingUnliftAll
    du tmr

discardingWRunner ::
       forall t. MonadTransUnliftAll t
    => WUnliftAll MonadUnliftIO t
    -> WUnliftAll MonadUnliftIO t
discardingWRunner (MkWUnliftAll u) = MkWUnliftAll $ discardingRunner u

outsideIn ::
       forall t m r. (MonadTransUnliftAll t, Monad m)
    => (forall a. (forall mm b. Monad mm => t mm (r, b) -> mm (a, b)) -> m a)
    -> t m r
outsideIn call = insideOut $ \unlift -> call $ \tmmrb -> fmap (\((r, b1), b) -> ((r, b), b1)) $ unlift tmmrb

-- | Swap two transformers in a transformer stack
commuteT ::
       forall ta tb m. (MonadTransUnliftAll ta, MonadTransUnliftAll tb, Monad m)
    => MFunction (ta (tb m)) (tb (ta m))
commuteT abmr =
    case hasTransConstraint @Monad @ta @m of
        Dict ->
            case hasTransConstraint @Monad @tb @m of
                Dict -> outsideIn $ \untb -> insideOut $ \unta -> untb $ unta abmr

commuteTBack ::
       forall ta tb m. (MonadTransUnliftAll ta, MonadTransUnliftAll tb, Monad m)
    => MBackFunction (ta (tb m)) (tb (ta m))
commuteTBack call = commuteT $ call commuteT

-- | Swap two transformers in a transformer stack (different generality)
commuteTUnliftIO ::
       forall ta tb m. (MonadTransTunnel ta, TransConstraint MonadIO ta, MonadTransUnliftAll tb, MonadTunnelIO m)
    => MFunction (ta (tb m)) (tb (ta m))
commuteTUnliftIO tatbmr =
    case hasTransConstraint @MonadIO @ta @m of
        Dict ->
            case hasTransConstraint @Functor @tb @m of
                Dict -> liftWithUnliftAll $ \unlift -> remonad' unlift tatbmr

type IOFunction m = MFunction m IO

type WIOFunction m = WMFunction m IO

mVarWIORun :: MVar s -> WIOFunction (StateT s IO)
mVarWIORun var = MkWMFunction $ mVarRun var

composeUnliftAllFunction ::
       (MonadTransUnliftAll t, MonadUnliftIO m) => UnliftAll MonadUnliftIO t -> MFunction m n -> MFunction (t m) n
composeUnliftAllFunction rt rm tma = rm $ rt tma

composeUnliftAllFunctionCommute ::
       (MonadTransUnliftAll t, MonadUnliftIO m, MonadUnliftIO n)
    => UnliftAll MonadUnliftIO t
    -> MFunction m n
    -> MFunction (t m) n
composeUnliftAllFunctionCommute rt rm tma = rt $ remonad rm tma

class (MonadFail m, MonadIO m, MonadFix m, MonadTunnelIO m) => MonadUnliftIO m where
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

instance MonadTransUnlift t => TransConstraint MonadUnliftIO t where
    hasTransConstraint =
        withTransConstraintDict @MonadFail $ withTransConstraintDict @MonadIO $ withTransConstraintDict @MonadFix $ Dict

instance MonadTransUnlift IdentityT

instance MonadTransUnliftAll IdentityT where
    insideOut call = IdentityT $ fmap fst $ call $ fmap (\a -> (a, ())) . runIdentityT
    liftWithUnliftAll call = IdentityT $ call runIdentityT
    getDiscardingUnliftAll = return identityWUnliftAll

instance MonadTransUnlift (ReaderT s)

instance MonadTransUnliftAll (ReaderT s) where
    insideOut call = ReaderT $ \s -> fmap fst $ call $ \(ReaderT smr) -> fmap (\a -> (a, ())) $ smr s
    liftWithUnliftAll call = ReaderT $ \s -> call $ \(ReaderT smr) -> smr s
    getDiscardingUnliftAll = do
        s <- ask
        return $ MkWUnliftAll $ \mr -> runReaderT mr s

instance Monoid s => MonadTransUnlift (WriterT s)

writerDiscardingUntrans :: UnliftAll MonadTunnelIO (WriterT s)
writerDiscardingUntrans mr = do
    (r, _discarded) <- runWriterT mr
    return r

instance Monoid s => MonadTransUnliftAll (WriterT s) where
    insideOut call = WriterT $ call runWriterT
    liftWithUnliftAll call = do
        var <- liftIO $ newMVar mempty
        r <-
            lift $
            call $ \(WriterT mrs) -> do
                (r, output) <- mrs
                liftIO $ modifyMVar_ var $ \oldoutput -> return $ mappend oldoutput output
                return r
        totaloutput <- liftIO $ takeMVar var
        tell totaloutput
        return r
    getDiscardingUnliftAll = return $ MkWUnliftAll writerDiscardingUntrans

instance MonadTransUnlift (StateT s)

stateDiscardingUntrans :: s -> UnliftAll MonadIO (StateT s)
stateDiscardingUntrans s mr = do
    (r, _discarded) <- runStateT mr s
    return r

instance MonadTransUnliftAll (StateT s) where
    insideOut call = StateT $ \olds -> call $ \(StateT smas) -> smas olds
    liftWithUnliftAll call = liftWithMVarStateT $ \var -> call $ mVarRun var
    getDiscardingUnliftAll = do
        s <- get
        return $ MkWUnliftAll $ stateDiscardingUntrans s
