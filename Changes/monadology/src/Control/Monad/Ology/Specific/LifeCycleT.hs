module Control.Monad.Ology.Specific.LifeCycleT
    ( LifeState(..)
    , LifeCycleT(..)
    , lifeCycleOnCloseIO
    , lifeCycleOnClose
    , forkLifeCycleT
    , getLifeState
    , runLifeCycleT
    , With
    , lifeCycleWith
    , lifeCycleMonitor
    , lifeCycleGetCloser
    , lifeCycleOnAllDone
    , LifeCycle
    ) where

import Control.Monad.Ology.General
import Control.Monad.Ology.Specific.CoroutineT
import Control.Monad.Ology.Specific.StateT
import Import

newtype LifeState = MkLifeState
    { closeLifeState :: IO ()
    }

instance Semigroup LifeState where
    MkLifeState p <> MkLifeState q = MkLifeState $ p >> q

instance Monoid LifeState where
    mempty = MkLifeState $ return ()

newtype LifeCycleT m a = MkLifeCycleT
    { unLifeCycleT :: MVar LifeState -> m a
    }

instance Functor m => Functor (LifeCycleT m) where
    fmap ab (MkLifeCycleT f) = MkLifeCycleT $ \var -> fmap ab $ f var

instance TransConstraint Functor LifeCycleT where
    hasTransConstraint = Dict

instance Applicative m => Applicative (LifeCycleT m) where
    pure t = MkLifeCycleT $ \_ -> pure t
    (MkLifeCycleT ocab) <*> (MkLifeCycleT oca) = MkLifeCycleT $ \var -> ocab var <*> oca var

instance TransConstraint Applicative LifeCycleT where
    hasTransConstraint = Dict

instance Monad m => Monad (LifeCycleT m) where
    return = pure
    (MkLifeCycleT va) >>= f =
        MkLifeCycleT $ \var -> do
            a <- va var
            unLifeCycleT (f a) var

instance TransConstraint Monad LifeCycleT where
    hasTransConstraint = Dict

instance MonadTrans LifeCycleT where
    lift ma = MkLifeCycleT $ \_ -> ma

instance MonadFail m => MonadFail (LifeCycleT m) where
    fail s = lift $ fail s

instance TransConstraint MonadFail LifeCycleT where
    hasTransConstraint = Dict

instance MonadException m => MonadException (LifeCycleT m) where
    type Exc (LifeCycleT m) = Exc m
    throwExc e = lift $ throwExc e
    catchExc :: forall a. LifeCycleT m a -> (Exc m -> LifeCycleT m a) -> LifeCycleT m a
    catchExc (MkLifeCycleT f) handler = MkLifeCycleT $ \var -> catchExc (f var) $ \e -> unLifeCycleT (handler e) var

instance TransConstraint MonadException LifeCycleT where
    hasTransConstraint = Dict

instance MonadThrow e m => MonadThrow e (LifeCycleT m) where
    throw e = lift $ throw e

instance TransConstraint (MonadThrow e) LifeCycleT where
    hasTransConstraint = Dict

instance MonadCatch e m => MonadCatch e (LifeCycleT m) where
    catch (MkLifeCycleT f) handler = MkLifeCycleT $ \var -> catch (f var) $ \e -> unLifeCycleT (handler e) var

instance TransConstraint (MonadCatch e) LifeCycleT where
    hasTransConstraint = Dict

instance MonadFix m => MonadFix (LifeCycleT m) where
    mfix f = MkLifeCycleT $ \var -> mfix $ \a -> unLifeCycleT (f a) var

instance TransConstraint MonadFix LifeCycleT where
    hasTransConstraint = Dict

instance MonadIO m => MonadIO (LifeCycleT m) where
    liftIO ioa = lift $ liftIO ioa

instance TransConstraint MonadIO LifeCycleT where
    hasTransConstraint = Dict

instance MonadTransHoist LifeCycleT where
    hoist f (MkLifeCycleT g) = MkLifeCycleT $ \var -> f $ g var

instance MonadTransTunnel LifeCycleT where
    type Tunnel LifeCycleT = Identity
    tunnel ::
           forall m r. Monad m
        => ((forall m1 a. Monad m1 => LifeCycleT m1 a -> m1 (Identity a)) -> m (Identity r))
        -> LifeCycleT m r
    tunnel f = MkLifeCycleT $ \var -> fmap runIdentity $ f $ \a -> fmap Identity $ unLifeCycleT a var

instance MonadTransUnlift LifeCycleT where
    liftWithUnlift call = MkLifeCycleT $ \var -> call $ \(MkLifeCycleT f) -> f var
    getDiscardingUnlift =
        return $
        MkWUnlift $ \(MkLifeCycleT f) -> do
            var <- liftIO $ newMVar mempty
            f var

lifeCycleOnCloseIO :: MonadIO m => IO () -> LifeCycleT m ()
lifeCycleOnCloseIO closer =
    MkLifeCycleT $ \var -> do
        dangerousMVarRun var $ do
            s <- get
            put $ MkLifeState closer <> s

lifeCycleOnClose :: MonadAskUnliftIO m => m () -> LifeCycleT m ()
lifeCycleOnClose closer = do
    MkWMFunction unlift <- lift askUnliftIO
    lifeCycleOnCloseIO $ unlift closer

type With m t = forall (r :: Type). (t -> m r) -> m r

withLifeCycleT ::
       forall m a. (MonadException m, MonadTunnelIO m)
    => LifeCycleT m a
    -> With m a
withLifeCycleT (MkLifeCycleT f) run = do
    var <- liftIO $ newMVar mempty
    finally (f var >>= run) $ do
        MkLifeState closer <- liftIO $ takeMVar var
        liftIO closer

runLifeCycleT ::
       forall m. (MonadException m, MonadTunnelIO m)
    => LifeCycleT m --> m
runLifeCycleT lc = withLifeCycleT lc return

forkLifeCycleT :: MonadUnliftIO m => m () -> LifeCycleT m ThreadId
forkLifeCycleT action = do
    var <- liftIO newEmptyMVar
    lifeCycleOnCloseIO $ takeMVar var
    lift $ liftIOWithUnlift $ \unlift -> forkIO $ finally (unlift action) $ putMVar var ()

getLifeState ::
       forall m a. MonadIO m
    => LifeCycleT m a
    -> m (a, LifeState)
getLifeState (MkLifeCycleT f) = do
    var <- liftIO $ newMVar mempty
    t <- f var
    ls <- liftIO $ takeMVar var
    return (t, ls)

-- | Runs the given lifecycle, returning a closer.
-- The closer is an idempotent action that will close the lifecycle only if it hasn't already been closed.
-- The closer will also be run as the closer of the resulting lifecycle.
lifeCycleGetCloser ::
       forall m a. MonadIO m
    => LifeCycleT m a
    -> LifeCycleT m (a, IO ())
lifeCycleGetCloser lc = do
    (a, MkLifeState closer) <- lift $ getLifeState lc
    var <- liftIO $ newMVar ()
    let
        earlycloser :: IO ()
        earlycloser = do
            mu <- tryTakeMVar var
            case mu of
                Just () -> closer
                Nothing -> return ()
    lifeCycleOnCloseIO earlycloser
    return (a, earlycloser)

-- | Returned action returns True if still alive, False if closed.
lifeCycleMonitor :: MonadIO m => LifeCycleT m (IO Bool)
lifeCycleMonitor = do
    ref <- liftIO $ newIORef True
    lifeCycleOnCloseIO $ writeIORef ref False
    return $ readIORef ref

lifeCycleOnAllDone ::
       forall m. MonadAskUnliftIO m
    => m ()
    -> m (LifeCycleT m (), m ())
lifeCycleOnAllDone onzero = do
    var <- liftIO $ newMVar (0 :: Int)
    let
        ondone = do
            liftIO $
                mVarRun var $ do
                    olda <- get
                    put $ succ olda
            lifeCycleOnClose $ do
                iszero <-
                    mVarRun var $ do
                        olda <- get
                        let newa = pred olda
                        put newa
                        return $ newa == 0
                if iszero
                    then onzero
                    else return ()
        checkdone = do
            iszero <-
                mVarRun var $ do
                    a <- get
                    return $ a == 0
            if iszero
                then onzero
                else return ()
    return (ondone, checkdone)

lifeCycleWith :: (MonadCoroutine m, MonadAskUnliftIO m) => With m t -> LifeCycleT m t
lifeCycleWith withX = do
    etp <- lift $ resume $ suspend withX
    case etp of
        Left t -> return t
        Right (t, tp) -> do
            lifeCycleOnClose $ do
                _ <- coroutineRun $ tp t
                return ()
            return t

type LifeCycle = LifeCycleT IO
