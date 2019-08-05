module Control.Monad.Trans.LifeCycle
    ( LifeState
    , closeLifeState
    , LifeCycleT(..)
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
import Data.IORef
import Shapes.Import

type LifeState m t = (t, m ())

newtype LifeCycleT m t = MkLifeCycleT
    { getLifeState :: m (LifeState m t)
    }

closeLifeState :: LifeState m t -> m ()
closeLifeState = snd

instance Functor m => Functor (LifeCycleT m) where
    fmap ab (MkLifeCycleT oc) = MkLifeCycleT $ fmap (\(a, cl) -> (ab a, cl)) oc

instance Monad m => Applicative (LifeCycleT m) where
    pure t = MkLifeCycleT $ return (t, return ())
    (MkLifeCycleT ocab) <*> (MkLifeCycleT oca) =
        MkLifeCycleT $ do
            (ab, clab) <- ocab
            (a, cla) <- oca
            return (ab a, cla >> clab)

instance Monad m => Monad (LifeCycleT m) where
    return = pure
    (MkLifeCycleT ioac) >>= f =
        MkLifeCycleT $ do
            (a, c1) <- ioac
            (b, c2) <- getLifeState $ f a
            return (b, c2 >> c1)

instance MonadFail m => MonadFail (LifeCycleT m) where
    fail s = MkLifeCycleT $ fail s

instance MonadFix m => MonadFix (LifeCycleT m) where
    mfix f = MkLifeCycleT $ mfix $ \ ~(t, _) -> getLifeState $ f t

instance MonadIO m => MonadIO (LifeCycleT m) where
    liftIO ma =
        MkLifeCycleT $ do
            a <- liftIO ma
            return (a, return ())

instance MonadTransConstraint Monad LifeCycleT where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadIO LifeCycleT where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadFail LifeCycleT where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadFix LifeCycleT where
    hasTransConstraint = Dict

instance MonadTrans LifeCycleT where
    lift ma = MkLifeCycleT $ fmap (\a -> (a, return ())) ma

instance MonadTransSemiTunnel LifeCycleT where
    semitunnel call = MkLifeCycleT $ call $ \(MkLifeCycleT m1r) -> fmap (fmap $ \m1u -> call $ \_ -> m1u) m1r

instance MonadTransSemiUnlift LifeCycleT where
    liftWithSemiUnlift call = do
        var <- liftIO $ newMVar $ return ()
        r <-
            lift $
            call $
            MkTransform $ \(MkLifeCycleT ma) -> do
                (a, closer) <- ma
                liftIO $ modifyMVar_ var $ \oldcloser -> return $ closer >> oldcloser
                return a
        totalcloser <- liftIO $ takeMVar var
        lifeCycleClose totalcloser
        return r
    getDiscardingSemiUnlift ::
           forall m. MonadUnliftIO m
        => LifeCycleT m (Transform (LifeCycleT m) m)
    getDiscardingSemiUnlift = return $ MkTransform $ \(MkLifeCycleT ms) -> fmap fst ms

lifeCycleClose :: Monad m => m () -> LifeCycleT m ()
lifeCycleClose closer = MkLifeCycleT $ return ((), closer)

type With m t = forall r. (t -> m r) -> m r

withLifeCycle :: MonadUnliftIO m => LifeCycleT m t -> With m t
withLifeCycle (MkLifeCycleT oc) run = do
    (t, closer) <- oc
    liftIOWithUnlift $ \(MkTransform unlift) -> finally (unlift $ run t) (unlift closer)

runLifeCycle :: MonadUnliftIO m => LifeCycleT m t -> m t
runLifeCycle lc = withLifeCycle lc return

lifeCycleWith :: (MonadCoroutine m, MonadFail m) => With m t -> LifeCycleT m t
lifeCycleWith withX =
    MkLifeCycleT $ do
        e1 <- resume $ suspend withX
        case e1 of
            Left _ -> fail "lifeCycleWith: not called"
            Right (t, as) ->
                return
                    ( t
                    , do
                          e2 <- resume $ as ()
                          case e2 of
                              Left () -> return ()
                              Right _ -> fail "lifeCycleWith: called twice")

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
lifeCycleEarlyCloser (MkLifeCycleT lc) =
    MkLifeCycleT $ do
        (a, closer) <- lc
        var <- liftIO $ newMVar ()
        let
            earlycloser :: m ()
            earlycloser = do
                mu <- liftIO $ tryTakeMVar var
                case mu of
                    Just () -> closer
                    Nothing -> return ()
        return ((a, earlycloser), earlycloser)

lifeCycleOnAllDone :: MonadUnliftIO m => m () -> m (LifeCycleT m (), m ())
lifeCycleOnAllDone onzero = do
    var <- liftIO $ newMVar (0 :: Int)
    let
        ondone = do
            liftIO $
                mvarRun var $ do
                    olda <- get
                    put $ succ olda
            lifeCycleClose $ do
                iszero <-
                    mvarRun var $ do
                        olda <- get
                        let newa = pred olda
                        put newa
                        return $ newa == 0
                if iszero
                    then onzero
                    else return ()
        checkdone = do
            iszero <-
                mvarRun var $ do
                    a <- get
                    return $ a == 0
            if iszero
                then onzero
                else return ()
    return (ondone, checkdone)
