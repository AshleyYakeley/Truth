module Control.Monad.Trans.LifeCycle
    ( LifeState
    , closeLifeState
    , LifeCycleT
    , getLifeState
    , lifeCycleClose
    , runLifeCycle
    , With
    , withLifeCycle
    , lifeCycleWith
    , lifeCycleMonitor
    , lifeCycleEarlyCloser
    , lifeCycleOnAllDone
    ) where

import Control.Monad.Coroutine
import Control.Monad.Exception
import Data.Coercion
import Data.IORef
import Shapes.Import

newtype LifeState m = MkLifeState
    { closeLifeState :: m ()
    }

instance Monad m => Semigroup (LifeState m) where
    MkLifeState p <> MkLifeState q = MkLifeState $ p >> q

instance Monad m => Monoid (LifeState m) where
    mempty = MkLifeState $ return ()

newtype LifeCycleT m t = MkLifeCycleT
    { unLifeCycleT :: MVar (LifeState m) -> m t
    }

instance RepresentationalRole LifeCycleT where
    representationalCoercion MkCoercion = MkCoercion

instance RepresentationalRole m => RepresentationalRole (LifeCycleT m) where
    representationalCoercion cab =
        case representationalCoercion @_ @_ @m cab of
            MkCoercion -> MkCoercion

getLifeState :: MonadIO m => LifeCycleT m t -> m (t, LifeState m)
getLifeState (MkLifeCycleT f) = do
    var <- liftIO $ newMVar mempty
    t <- f var
    ls <- liftIO $ takeMVar var
    return (t, ls)

instance Functor m => Functor (LifeCycleT m) where
    fmap ab (MkLifeCycleT f) = MkLifeCycleT $ \var -> fmap ab $ f var

instance Applicative m => Applicative (LifeCycleT m) where
    pure t = MkLifeCycleT $ \_ -> pure t
    (MkLifeCycleT ocab) <*> (MkLifeCycleT oca) = MkLifeCycleT $ \var -> ocab var <*> oca var

instance Monad m => Monad (LifeCycleT m) where
    return = pure
    (MkLifeCycleT va) >>= f =
        MkLifeCycleT $ \var -> do
            a <- va var
            unLifeCycleT (f a) var

instance MonadFail m => MonadFail (LifeCycleT m) where
    fail s = lift $ fail s

instance MonadFix m => MonadFix (LifeCycleT m) where
    mfix f = MkLifeCycleT $ \var -> mfix $ \a -> unLifeCycleT (f a) var

instance MonadIO m => MonadIO (LifeCycleT m) where
    liftIO ioa = lift $ liftIO ioa

instance MonadTransConstraint Monad LifeCycleT where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadIO LifeCycleT where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadFail LifeCycleT where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadFix LifeCycleT where
    hasTransConstraint = Dict

instance MonadTrans LifeCycleT where
    lift ma = MkLifeCycleT $ \_ -> ma

instance MonadTransUnlift LifeCycleT where
    liftWithUnlift call = MkLifeCycleT $ \var -> call $ \(MkLifeCycleT f) -> f var
    getDiscardingUnlift =
        return $
        MkWMFunction $ \(MkLifeCycleT f) -> do
            var <- liftIO $ newMVar mempty
            f var

lifeCycleClose :: MonadIO m => m () -> LifeCycleT m ()
lifeCycleClose closer =
    MkLifeCycleT $ \var ->
        dangerousMVarRun var $ do
            s <- get
            put $ MkLifeState closer <> s

type With m t = forall r. (t -> m r) -> m r

withLifeCycle :: (MonadBracket m, MonadIO m) => LifeCycleT m t -> With m t
withLifeCycle (MkLifeCycleT f) run = do
    var <- liftIO $ newMVar mempty
    finally (f var >>= run) $ do
        MkLifeState closer <- liftIO $ takeMVar var
        closer

runLifeCycle :: (MonadBracket m, MonadIO m) => LifeCycleT m t -> m t
runLifeCycle lc = withLifeCycle lc return

lifeCycleWith :: (MonadCoroutine m, MonadFail m, MonadIO m) => With m t -> LifeCycleT m t
lifeCycleWith withX = do
    etp <- lift $ resume $ suspend withX
    case etp of
        Left t -> return t
        Right (t, tp) -> do
            lifeCycleClose $ do
                _ <- runSuspendedUntilDone $ tp t
                return ()
            return t

-- | Returned action returns True if still alive, False if closed.
lifeCycleMonitor :: MonadIO m => LifeCycleT m (IO Bool)
lifeCycleMonitor = do
    ref <- liftIO $ newIORef True
    lifeCycleClose $ liftIO $ writeIORef ref False
    return $ readIORef ref

-- | Runs the given lifecycle, returning an early closer.
-- The early closer is an idempotent action that will close the lifecycle only if it hasn't already been closed.
-- The early closer will also be run as the closer of the resulting lifecycle.
lifeCycleEarlyCloser ::
       forall m a. MonadIO m
    => LifeCycleT m a
    -> LifeCycleT m (a, m ())
lifeCycleEarlyCloser lc = do
    (a, MkLifeState closer) <- lift $ getLifeState lc
    var <- liftIO $ newMVar ()
    let
        earlycloser :: m ()
        earlycloser = do
            mu <- liftIO $ tryTakeMVar var
            case mu of
                Just () -> closer
                Nothing -> return ()
    lifeCycleClose earlycloser
    return (a, earlycloser)

lifeCycleOnAllDone :: MonadUnliftIO m => m () -> m (LifeCycleT m (), m ())
lifeCycleOnAllDone onzero = do
    var <- liftIO $ newMVar (0 :: Int)
    let
        ondone = do
            liftIO $
                mVarRun var $ do
                    olda <- get
                    put $ succ olda
            lifeCycleClose $ do
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
