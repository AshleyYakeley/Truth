module Control.Monad.LifeCycle
    ( LifeState
    , closeLifeState
    , LifeCycle
    , MonadLifeCycleIO(..)
    , lifeCycleClose
    , MonadUnliftLifeCycleIO(..)
    , LiftLifeCycle(..)
    , lifeCycleCloseInner
    , runLifeCycle
    , With
    , lifeCycleWith
    , lifeCycleMonitor
    , lifeCycleEarlyCloser
    , lifeCycleOnAllDone
    ) where

import Control.Monad.Coroutine
import Control.Monad.Ology.Exception
import Data.Coercion
import Data.IORef
import Shapes.Import
import Debug.ThreadTrace

newtype LifeState = MkLifeState
    { closeLifeState :: IO ()
    }

instance Semigroup LifeState where
    MkLifeState p <> MkLifeState q = MkLifeState $ p >> q

instance Monoid LifeState where
    mempty = MkLifeState $ return ()

newtype LifeCycle t = MkLifeCycle
    { unLifeCycleT :: MVar LifeState -> IO t
    }

instance RepresentationalRole LifeCycle where
    representationalCoercion MkCoercion = MkCoercion

instance Functor LifeCycle where
    fmap ab (MkLifeCycle f) = MkLifeCycle $ \var -> fmap ab $ f var

instance Applicative LifeCycle where
    pure t = MkLifeCycle $ \_ -> pure t
    (MkLifeCycle ocab) <*> (MkLifeCycle oca) = MkLifeCycle $ \var -> ocab var <*> oca var

instance Monad LifeCycle where
    return = pure
    (MkLifeCycle va) >>= f =
        MkLifeCycle $ \var -> do
            a <- va var
            unLifeCycleT (f a) var

instance MonadFail LifeCycle where
    fail s = liftIO $ fail s

instance MonadThrow e IO => MonadThrow e LifeCycle where
    throw e = liftIO $ throw e

instance MonadFix LifeCycle where
    mfix f = MkLifeCycle $ \var -> mfix $ \a -> unLifeCycleT (f a) var

instance MonadIO LifeCycle where
    liftIO ioa = MkLifeCycle $ \_ -> ioa

instance MonadTunnelIO LifeCycle where
    type TunnelIO LifeCycle = Identity
    tunnelIO :: forall r. ((forall a. LifeCycle a -> IO (Identity a)) -> IO (Identity r)) -> LifeCycle r
    tunnelIO f = MkLifeCycle $ \var -> fmap runIdentity $ f $ \a -> fmap Identity $ unLifeCycleT a var

instance MonadUnliftIO LifeCycle where
    liftIOWithUnlift call = MkLifeCycle $ \var -> call $ \(MkLifeCycle f) -> f var
    getDiscardingIOUnlift =
        return $
        MkWMFunction $ \(MkLifeCycle f) -> do
            var <- liftIO $ newMVar mempty
            f var

type With m t = forall r. (t -> m r) -> m r

class MonadIO m => MonadLifeCycleIO m where
    liftLifeCycle :: forall a. LifeCycle a -> m a
    subLifeCycle :: forall a. m a -> m a
    default subLifeCycle :: forall a. LiftLifeCycle m => m a -> m a
    subLifeCycle lc = liftToLifeCycle $ runLifeCycle lc

lifeCycleClose :: MonadLifeCycleIO m => IO () -> m ()
lifeCycleClose closer =
    liftLifeCycle $
    MkLifeCycle $ \var ->
        dangerousMVarRun var $ do
            s <- get
            put $ MkLifeState (traceBracketIO "closer" closer) <> s

instance {-# OVERLAPPING #-} MonadLifeCycleIO LifeCycle where
    liftLifeCycle lc = lc

instance (TransTunnel t, MonadIO (t m), MonadLifeCycleIO m) => MonadLifeCycleIO (t m) where
    liftLifeCycle lc = lift $ liftLifeCycle lc
    subLifeCycle = hoist subLifeCycle

instance (TransTunnel t, TransConstraint MonadIO t) => TransConstraint MonadLifeCycleIO t where
    hasTransConstraint ::
           forall m. MonadLifeCycleIO m
        => Dict (MonadLifeCycleIO (t m))
    hasTransConstraint =
        case hasTransConstraint @MonadIO @t @m of
            Dict -> Dict

class MonadLifeCycleIO m => MonadUnliftLifeCycleIO m where
    liftLifeCycleIOWithUnlift :: LifeCycle -/-> m

getLifeState :: MonadUnliftLifeCycleIO m => m a -> m (a, LifeState)
getLifeState ma = liftLifeCycleIOWithUnlift $ \unlift -> liftIO $ getInnerLifeState $ unlift ma

-- | Runs the given lifecycle, returning an early closer.
-- The early closer is an idempotent action that will close the lifecycle only if it hasn't already been closed.
-- The early closer will also be run as the closer of the resulting lifecycle.
lifeCycleEarlyCloser ::
       forall m a. MonadUnliftLifeCycleIO m
    => m a
    -> m (a, IO ())
lifeCycleEarlyCloser lc = do
    (a, MkLifeState closer) <- getLifeState lc
    var <- liftIO $ newMVar ()
    let
        earlycloser :: IO ()
        earlycloser = do
            mu <- tryTakeMVar var
            case mu of
                Just () -> closer
                Nothing -> return ()
    lifeCycleClose $ traceBracketIO "lifeCycleEarlyCloser.close" earlycloser
    return (a, earlycloser)

instance MonadUnliftLifeCycleIO LifeCycle where
    liftLifeCycleIOWithUnlift call = call id

instance (MonadUnliftLifeCycleIO m, MonadUnliftIO m, TransTunnel t, MonadTransUnlift t, MonadIO (t m)) =>
             MonadUnliftLifeCycleIO (t m) where
    liftLifeCycleIOWithUnlift call =
        liftWithUnlift $ \unlift -> liftLifeCycleIOWithUnlift $ \unliftLC -> call $ unliftLC . unlift

type LiftLifeCycle :: (Type -> Type) -> Constraint
class (MonadLifeCycleIO m, MonadIO (LifeCycleInner m)) => LiftLifeCycle m where
    type LifeCycleInner m :: Type -> Type
    liftToLifeCycle :: forall a. LifeCycleInner m a -> m a
    getInnerLifeState :: forall a. m a -> LifeCycleInner m (a, LifeState)
    withLifeCycle :: forall a. m a -> With (LifeCycleInner m) a

lifeCycleCloseInner :: (LiftLifeCycle m, MonadAskUnliftIO (LifeCycleInner m)) => LifeCycleInner m () -> m ()
lifeCycleCloseInner closer = do
    MkWMFunction unlift <- liftToLifeCycle askUnliftIO
    lifeCycleClose $ traceBracketIO "lifeCycleCloseInner.close" $ unlift closer

runLifeCycle :: LiftLifeCycle m => m t -> LifeCycleInner m t
runLifeCycle lc = withLifeCycle lc return

instance LiftLifeCycle LifeCycle where
    type LifeCycleInner LifeCycle = IO
    liftToLifeCycle = liftIO
    getInnerLifeState (MkLifeCycle f) = do
        var <- newMVar mempty
        t <- f var
        ls <- takeMVar var
        return (t, ls)
    withLifeCycle (MkLifeCycle f) run = do
        var <- newMVar mempty
        finally (f var >>= run) $ traceBracketIO "withLifeCycle closing" $ do
            MkLifeState closer <- takeMVar var
            closer

instance ( MonadTransUnlift t
         , LiftLifeCycle m
         , MonadUnliftIO m
         , MonadUnliftIO (LifeCycleInner m)
         , MonadIO (t m)
         , MonadIO (t (LifeCycleInner m))
         ) => LiftLifeCycle (t m) where
    type LifeCycleInner (t m) = t (LifeCycleInner m)
    liftToLifeCycle = hoist liftToLifeCycle
    getInnerLifeState tma = liftWithUnlift $ \unlift -> getInnerLifeState $ unlift tma
    withLifeCycle ma call = liftWithUnlift $ \unlift -> withLifeCycle (unlift ma) (\a -> unlift $ call a)

lifeCycleWith ::
       (MonadCoroutine (LifeCycleInner m), MonadAskUnliftIO (LifeCycleInner m), LiftLifeCycle m)
    => With (LifeCycleInner m) t
    -> m t
lifeCycleWith withX = do
    etp <- liftToLifeCycle $ resume $ suspend withX
    case etp of
        Left t -> return t
        Right (t, tp) -> do
            lifeCycleCloseInner $ do
                _ <- runSuspendedUntilDone $ tp t
                return ()
            return t

-- | Returned action returns True if still alive, False if closed.
lifeCycleMonitor :: LiftLifeCycle m => m (IO Bool)
lifeCycleMonitor = do
    ref <- liftIO $ newIORef True
    lifeCycleClose $ writeIORef ref False
    return $ readIORef ref

lifeCycleOnAllDone ::
       (LiftLifeCycle m, MonadAskUnliftIO (LifeCycleInner m))
    => LifeCycleInner m ()
    -> LifeCycleInner m (m (), LifeCycleInner m ())
lifeCycleOnAllDone onzero = do
    var <- liftIO $ newMVar (0 :: Int)
    let
        ondone = do
            liftIO $
                mVarRun var $ do
                    olda <- get
                    put $ succ olda
            lifeCycleCloseInner $ do
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
