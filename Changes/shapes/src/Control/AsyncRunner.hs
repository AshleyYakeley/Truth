module Control.AsyncRunner
    ( asyncWaitRunner
    , asyncRunner
    , asyncIORunner
    ) where

import Control.Task
import Shapes.Import

asyncIORunner :: Text -> Lifecycle (IO () -> IO (), Task IO ())
asyncIORunner _ = do
    var <- liftIO $ newMVar mempty
    let
        pushVal :: IO () -> IO ()
        pushVal job =
            mVarRunStateT var $ do
                oldTask <- get
                newTask <-
                    lift $
                    forkTask $ do
                        taskWait oldTask
                        job
                put newTask
        utask :: Task IO ()
        utask = ioTask $ mVarRunStateT var get
    lifecycleOnClose $ taskWait utask
    return (pushVal, utask)

newtype SemigroupQueue t =
    MkSemigroupQueue (MVar (Maybe t))

newSemigroupQueue :: IO (SemigroupQueue t)
newSemigroupQueue = do
    var <- newMVar Nothing
    return $ MkSemigroupQueue var

takeSemigroupQueue :: SemigroupQueue t -> IO (Maybe t)
takeSemigroupQueue (MkSemigroupQueue var) =
    mVarRunStateT var $ do
        mt <- get
        put Nothing
        return mt

putSemigroupQueue :: Semigroup t => SemigroupQueue t -> t -> IO Bool
putSemigroupQueue (MkSemigroupQueue var) t =
    mVarRunStateT var $ do
        get >>= \case
            Just oldt -> do
                put $ Just $ oldt <> t
                return False
            Nothing -> do
                put $ Just t
                return True

asyncRunner ::
       forall t. Semigroup t
    => Text
    -> (t -> IO ())
    -> Lifecycle (t -> IO (), Task IO ())
asyncRunner name doit = do
    sq :: SemigroupQueue t <- liftIO $ newSemigroupQueue
    (ioio, utask) <- asyncIORunner name
    let
        action :: IO ()
        action = do
            mt <- takeSemigroupQueue sq
            case mt of
                Just t -> doit t
                Nothing -> return ()
        tio :: t -> IO ()
        tio t = do
            b <- putSemigroupQueue sq t
            if b
                then ioio action
                else return ()
    return (tio, utask)

asyncWaitRunner ::
       forall t. Semigroup t
    => Text
    -> Int
    -> (t -> IO ())
    -> Lifecycle (Maybe t -> IO (), Task IO ())
asyncWaitRunner name mus doit = do
    sq :: SemigroupQueue t <- liftIO $ newSemigroupQueue
    (ioio, utask) <- asyncIORunner name
    let
        action :: IO ()
        action = do
            mt <- takeSemigroupQueue sq
            case mt of
                Just t -> doit t
                Nothing -> return ()
        tio :: Maybe t -> IO ()
        tio (Just t) = do
            b <- putSemigroupQueue sq t
            if b
                then ioio $ do
                         threadDelay mus
                         action
                else return ()
        tio Nothing = ioio action
    return (tio, utask)
