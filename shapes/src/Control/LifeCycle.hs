module Control.LifeCycle
    ( LifeCycle(..)
    , With
    , withLifeCycle
    , lifeCycleWith
    , deferrer
    ) where

import Shapes.Import

newtype LifeCycle t = MkLifeCycle
    { runLifeCycle :: IO (t, IO ())
    }

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

instance MonadFix LifeCycle where
    mfix f = MkLifeCycle $ mfix $ \ ~(t, _) -> runLifeCycle $ f t

instance MonadIO LifeCycle where
    liftIO ma =
        MkLifeCycle $ do
            a <- ma
            return (a, return ())

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

data VarState
    = VSEmpty
    | VSDo (IO ())
    | VSDone

deferrer :: LifeCycle (IO () -> IO ())
deferrer = do
    actionVar :: TVar VarState <- liftIO $ newTVarIO $ VSEmpty
    let
        threadDo :: IO ()
        threadDo = do
            maction <-
                atomically $ do
                    vs <- readTVar actionVar
                    case vs of
                        VSDone -> do
                            writeTVar actionVar $ VSEmpty
                            return Nothing
                        VSEmpty -> mzero
                        VSDo action -> do
                            writeTVar actionVar $ VSEmpty
                            return $ Just action
            case maction of
                Just action -> do
                    action
                    threadDo
                Nothing -> return ()
        waitForEmpty :: STM ()
        waitForEmpty = do
            vs <- readTVar actionVar
            case vs of
                VSEmpty -> return ()
                _ -> mzero
    _ <- liftIO $ forkIO threadDo
    lifeCycleClose $ do
        atomically $ do
            waitForEmpty
            writeTVar actionVar $ VSDone
        atomically waitForEmpty
    return $ \action ->
        atomically $ do
            vs <- readTVar actionVar
            case vs of
                VSDone -> return ()
                VSEmpty -> writeTVar actionVar $ VSDo action
                VSDo oldaction -> writeTVar actionVar $ VSDo $ oldaction >> action
