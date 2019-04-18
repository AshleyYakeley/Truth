module Control.LifeCycle
    ( LifeState
    , closeLifeState
    , LifeCycle(..)
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
import Debug.ThreadTrace

type LifeState t = (t, IO ())

newtype LifeCycle t = MkLifeCycle
    { getLifeState :: IO (LifeState t)
    }

closeLifeState :: LifeState t -> IO ()
closeLifeState = snd

instance Functor LifeCycle where
    fmap ab (MkLifeCycle oc) =
        MkLifeCycle $ do
            (a, closer) <- oc
            return (ab a, closer)

instance Applicative LifeCycle where
    pure t = MkLifeCycle $ return (t, return ())
    (MkLifeCycle ocab) <*> (MkLifeCycle oca) =
        MkLifeCycle $ do
            (ab, clab) <- ocab
            (a, cla) <- oca
            return (ab a, cla >> clab)

instance Monad LifeCycle where
    return = pure
    (MkLifeCycle ioac) >>= f =
        MkLifeCycle $ do
            (a, c1) <- ioac
            (b, c2) <- getLifeState $ f a
            return (b, c2 >> c1)

instance MonadFail LifeCycle where
    fail s = MkLifeCycle $ fail s

instance MonadFix LifeCycle where
    mfix f = MkLifeCycle $ mfix $ \ ~(t, _) -> getLifeState $ f t

instance MonadIO LifeCycle where
    liftIO ma =
        MkLifeCycle $ do
            a <- ma
            return (a, return ())

instance MonadTunnelIO LifeCycle where
    tunnelIO call = MkLifeCycle $ call getLifeState

instance MonadUnliftIO LifeCycle where
    liftIOWithUnlift call = do
        var <- liftIO $ newMVar mempty
        r <-
            liftIO $
            call $
            MkTransform $ \(MkLifeCycle mrs) -> do
                (r, closer) <- mrs
                liftIO $ modifyMVar var $ \oldcloser -> return (closer >> oldcloser, ())
                return r
        totalcloser <- liftIO $ takeMVar var
        lifeCycleClose totalcloser
        return r
    getDiscardingUnliftIO =
        return $
        MkTransform $ \mr -> do
            (r, _discarded) <- getLifeState mr
            return r

lifeCycleClose :: IO () -> LifeCycle ()
lifeCycleClose closer = MkLifeCycle $ return ((), closer)

type With t = forall r. (t -> IO r) -> IO r

withLifeCycle :: LifeCycle t -> With t
withLifeCycle (MkLifeCycle oc) run = do
    (t, closer) <- oc
    finally (run t) closer

runLifeCycle :: LifeCycle t -> IO t
runLifeCycle lc = withLifeCycle lc return

lifeCycleWith :: With t -> LifeCycle t
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
lifeCycleEarlyCloser :: LifeCycle a -> LifeCycle (a, IO ())
lifeCycleEarlyCloser (MkLifeCycle lc) =
    MkLifeCycle $ do
        (a, closer) <- lc
        var <- newMVar ()
        let
            earlycloser :: IO ()
            earlycloser = do
                mu <- tryTakeMVar var
                case mu of
                    Just () -> closer
                    Nothing -> return ()
        return ((a, earlycloser), earlycloser)

lifeCycleOnAllDone :: IO () -> IO (LifeCycle ())
lifeCycleOnAllDone onzero = do
    var <- newMVar (0 :: Int)
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
    -> LifeCycle (Maybe t -> IO ())
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
                    traceBracket "asyncWaitRunner: do" action
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
            traceBracket "asyncWaitRunner: push val" $
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
    -> LifeCycle (t -> IO ())
asyncRunner doit = fmap (\push -> push . Just) $ asyncWaitRunner 0 doit

asyncIORunner :: LifeCycle (IO () -> IO ())
asyncIORunner = fmap (\pushVal io -> pushVal [io]) $ asyncRunner sequence_
