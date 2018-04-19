module Control.LifeCycle
    ( LifeState
    , closeLifeState
    , LifeCycle(..)
    , lifeCycleClose
    , With
    , withLifeCycle
    , lifeCycleWith
    , MonadLifeCycle(..)
    , listDeferrer
    , deferrer
    ) where

import Shapes.Import

type LifeState t = (t, IO ())

newtype LifeCycle t = MkLifeCycle
    { runLifeCycle :: IO (LifeState t)
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
            (b, c2) <- runLifeCycle $ f a
            return (b, c2 >> c1)

instance MonadFail LifeCycle where
    fail s = MkLifeCycle $ fail s

instance MonadFix LifeCycle where
    mfix f = MkLifeCycle $ mfix $ \ ~(t, _) -> runLifeCycle $ f t

instance MonadIO LifeCycle where
    liftIO ma =
        MkLifeCycle $ do
            a <- ma
            return (a, return ())

instance MonadTunnelIO LifeCycle where
    tunnelIO call = MkLifeCycle $ call runLifeCycle

instance MonadUnliftIO LifeCycle where
    liftIOWithUnlift call = do
        var <- liftIO $ newMVar mempty
        r <-
            liftIO $
            call $
            MkUnliftIO $ \(MkLifeCycle mrs) -> do
                (r, closer) <- mrs
                liftIO $ modifyMVar var $ \oldcloser -> return (closer >> oldcloser, ())
                return r
        totalcloser <- liftIO $ takeMVar var
        lifeCycleClose totalcloser
        return r
    getDiscardingUnliftIO =
        return $
        MkUnliftIO $ \mr -> do
            (r, _discarded) <- runLifeCycle mr
            return r

lifeCycleClose :: IO () -> LifeCycle ()
lifeCycleClose closer = MkLifeCycle $ return ((), closer)

type With t = forall r. (t -> IO r) -> IO r

withLifeCycle :: LifeCycle t -> With t
withLifeCycle (MkLifeCycle oc) run = do
    (t, closer) <- oc
    finally (run t) closer

lifeCycleWith :: With t -> LifeCycle t
lifeCycleWith withX =
    MkLifeCycle $ do
        tVar <- newEmptyMVar
        closerVar <- newEmptyMVar
        doneVar <- newEmptyMVar
        _ <-
            forkIO $ do
                withX $ \t -> do
                    putMVar tVar t
                    takeMVar closerVar
                putMVar doneVar ()
        t <- takeMVar tVar
        let
            close :: IO ()
            close = do
                putMVar closerVar ()
                takeMVar doneVar
        return (t, close)

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
    | VSDo (NonEmpty t)
    | VSDone

listDeferrer :: forall t. (NonEmpty t -> IO ()) -> LifeCycle (t -> IO ())
listDeferrer doit = do
    bufferVar :: TVar (VarState t) <- liftIO $ newTVarIO $ VSEmpty
    let
        threadDo :: IO ()
        threadDo = do
            mvals <-
                atomically $ do
                    vs <- readTVar bufferVar
                    case vs of
                        VSDone -> do
                            writeTVar bufferVar $ VSEmpty
                            return Nothing
                        VSEmpty -> mzero
                        VSDo cur -> do
                            writeTVar bufferVar $ VSEmpty
                            return $ Just cur
            case mvals of
                Just vals -> do
                    doit vals
                    threadDo
                Nothing -> return ()
        waitForEmpty :: STM ()
        waitForEmpty = do
            vs <- readTVar bufferVar
            case vs of
                VSEmpty -> return ()
                _ -> mzero
    _ <- liftIO $ forkIO threadDo
    lifeCycleClose $ do
        atomically $ do
            waitForEmpty
            writeTVar bufferVar $ VSDone
        atomically waitForEmpty
    return $ \val ->
        atomically $ do
            vs <- readTVar bufferVar
            case vs of
                VSDone -> return ()
                VSEmpty -> writeTVar bufferVar $ VSDo $ opoint val
                VSDo oldval -> writeTVar bufferVar $ VSDo $ oldval <> opoint val

deferrer :: LifeCycle (IO () -> IO ())
deferrer = listDeferrer sequence_
