module Control.LifeCycle
    ( LifeState
    , closeLifeState
    , LifeCycle
    , LifeCycleT(..)
    , lifeCycleClose
    , runLifeCycle
    , With
    , withLifeCycle
    , lifeCycleWith
    , lifeCycleEarlyCloser
    , lifeCycleOnAllDone
    , MonadLifeCycle(..)
    , asyncWaitRunner
    , asyncRunner
    , asyncIORunner
    ) where

import Control.Monad.Coroutine
import Shapes.Import

type LifeState m t = (t, m ())

newtype LifeCycleT m t = MkLifeCycle
    { getLifeState :: m (LifeState m t)
    }

closeLifeState :: LifeState m t -> m ()
closeLifeState = snd

instance Functor m => Functor (LifeCycleT m) where
    fmap ab (MkLifeCycle oc) = MkLifeCycle $ fmap (\(a, cl) -> (ab a, cl)) oc

instance Monad m => Applicative (LifeCycleT m) where
    pure t = MkLifeCycle $ return (t, return ())
    (MkLifeCycle ocab) <*> (MkLifeCycle oca) =
        MkLifeCycle $ do
            (ab, clab) <- ocab
            (a, cla) <- oca
            return (ab a, cla >> clab)

instance Monad m => Monad (LifeCycleT m) where
    return = pure
    (MkLifeCycle ioac) >>= f =
        MkLifeCycle $ do
            (a, c1) <- ioac
            (b, c2) <- getLifeState $ f a
            return (b, c2 >> c1)

instance MonadFail m => MonadFail (LifeCycleT m) where
    fail s = MkLifeCycle $ fail s

instance MonadFix m => MonadFix (LifeCycleT m) where
    mfix f = MkLifeCycle $ mfix $ \ ~(t, _) -> getLifeState $ f t

instance MonadIO m => MonadIO (LifeCycleT m) where
    liftIO ma =
        MkLifeCycle $ do
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
    lift ma = MkLifeCycle $ fmap (\a -> (a, return ())) ma

instance MonadTransSemiTunnel LifeCycleT where
    semitunnel call = MkLifeCycle $ call $ \(MkLifeCycle m1r) -> fmap (fmap $ \m1u -> call $ \_ -> m1u) m1r

instance MonadTransSemiUnlift LifeCycleT where
    liftWithSemiUnlift call = do
        var <- liftIO $ newMVar $ return ()
        r <-
            lift $
            call $
            MkTransform $ \(MkLifeCycle ma) -> do
                (a, closer) <- ma
                liftIO $ modifyMVar_ var $ \oldcloser -> return $ closer >> oldcloser
                return a
        totalcloser <- liftIO $ takeMVar var
        lifeCycleClose totalcloser
        return r
    getDiscardingSemiUnlift ::
           forall m. MonadUnliftIO m
        => LifeCycleT m (Transform (LifeCycleT m) m)
    getDiscardingSemiUnlift = return $ MkTransform $ \(MkLifeCycle ms) -> fmap fst ms

lifeCycleClose :: Monad m => m () -> LifeCycleT m ()
lifeCycleClose closer = MkLifeCycle $ return ((), closer)

type With m t = forall r. (t -> m r) -> m r

withLifeCycle :: MonadUnliftIO m => LifeCycleT m t -> With m t
withLifeCycle (MkLifeCycle oc) run = do
    (t, closer) <- oc
    liftIOWithUnlift $ \(MkTransform unlift) -> finally (unlift $ run t) (unlift closer)

runLifeCycle :: MonadUnliftIO m => LifeCycleT m t -> m t
runLifeCycle lc = withLifeCycle lc return

lifeCycleWith :: (MonadCoroutine m, MonadFail m) => With m t -> LifeCycleT m t
lifeCycleWith withX =
    MkLifeCycle $ do
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

-- | Runs the given lifecycle, returning an early closer.
-- The early closer is an idempotent action that will close the lifecycle only if it hasn't already been closed.
-- The early closer will also be run as the closer of the resulting lifecycle.
lifeCycleEarlyCloser ::
       forall m a. MonadIO m
    => LifeCycleT m a
    -> LifeCycleT m (a, m ())
lifeCycleEarlyCloser (MkLifeCycle lc) =
    MkLifeCycle $ do
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

lifeCycleOnAllDone :: MonadUnliftIO m => m () -> m (LifeCycleT m ())
lifeCycleOnAllDone onzero = do
    var <- liftIO $ newMVar (0 :: Int)
    return $ do
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

type LifeCycle = LifeCycleT IO

class MonadIO m => MonadLifeCycle m where
    liftLifeCycle :: forall a. LifeCycle a -> m a

instance MonadLifeCycle LifeCycle where
    liftLifeCycle lc = lc

instance (MonadTrans t, MonadIO (t m), MonadLifeCycle m) => MonadLifeCycle (t m) where
    liftLifeCycle lc = lift $ liftLifeCycle lc

instance (MonadTrans t, MonadTransConstraint MonadIO t) => MonadTransConstraint MonadLifeCycle t where
    hasTransConstraint ::
           forall m. MonadLifeCycle m
        => Dict (MonadLifeCycle (t m))
    hasTransConstraint =
        case hasTransConstraint @MonadIO @t @m of
            Dict -> Dict

data VarState t
    = VSEmpty
    | VSDo t
           Bool
    | VSDone

asyncWaitRunner ::
       forall t. Semigroup t
    => Int
    -> (t -> IO ())
    -> LifeCycleT IO (Maybe t -> IO ())
asyncWaitRunner mus doit = do
    bufferVar :: TVar (VarState t) <- liftIO $ newTVarIO $ VSEmpty
    let
        threadDo :: IO ()
        threadDo = do
            maction <-
                atomically $ do
                    vs <- readTVar bufferVar
                    case vs of
                        VSDone -> do
                            writeTVar bufferVar $ VSEmpty
                            return Nothing
                        VSEmpty -> mzero
                        VSDo vals True
                            | mus > 0 -> do
                                writeTVar bufferVar $ VSDo vals False
                                return $ Just $ threadDelay mus
                        VSDo vals _ -> do
                            writeTVar bufferVar $ VSEmpty
                            return $ Just $ doit vals
            case maction of
                Just action -> do
                    action
                    threadDo
                Nothing -> return ()
        waitForEmpty :: STM ()
        waitForEmpty = do
            vs <- readTVar bufferVar
            case vs of
                VSEmpty -> return ()
                _ -> mzero
        pushVal :: Maybe t -> IO ()
        pushVal (Just val) =
            atomically $ do
                vs <- readTVar bufferVar
                case vs of
                    VSDone -> return ()
                    VSEmpty -> writeTVar bufferVar $ VSDo val True
                    VSDo oldval _ -> writeTVar bufferVar $ VSDo (oldval <> val) True
        pushVal Nothing =
            atomically $ do
                vs <- readTVar bufferVar
                case vs of
                    VSDo oldval False -> writeTVar bufferVar $ VSDo oldval True
                    _ -> return ()
    _ <- liftIO $ forkIO threadDo
    lifeCycleClose $ do
        atomically $ do
            waitForEmpty
            writeTVar bufferVar $ VSDone
        atomically waitForEmpty
    return pushVal

asyncRunner ::
       forall t. Semigroup t
    => (t -> IO ())
    -> LifeCycleT IO (t -> IO ())
asyncRunner doit = fmap (\push -> push . Just) $ asyncWaitRunner 0 doit

asyncIORunner :: LifeCycleT IO (IO () -> IO ())
asyncIORunner = fmap (\pushVal io -> pushVal [io]) $ asyncRunner sequence_
