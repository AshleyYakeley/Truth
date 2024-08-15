module Control.AsyncRunner
    ( asyncWaitRunner
    , asyncRunner
    , asyncIORunner
    ) where

import Control.Task
import Shapes.Import

data VarState t
    = VSIdle
    -- ^ worker is idle, with no tasks pending
    | VSException SomeException
    -- ^ worker has terminated because task gave this exception
    | VSRunning
    -- ^ worker is working, with no tasks pending
    | VSPending t
                Bool
    -- ^ worker has tasks pending, flag means worker should wait for more tasks (for given period)
    | VSEnd
    -- ^ caller is telling worker to terminate

-- | Useful for debugging
instance Show (VarState t) where
    show VSIdle = "idle"
    show (VSException e) = "exception " ++ show e
    show VSRunning = "running"
    show (VSPending _ True) = "pending (wait)"
    show (VSPending _ False) = "pending (no wait)"
    show VSEnd = "end"

asyncWaitRunner ::
       forall t. Semigroup t
    => Text
    -> Int
    -> (t -> IO ())
    -> Lifecycle (Maybe t -> IO (), Task IO ())
asyncWaitRunner _ mus doit = do
    bufferVar :: TVar (VarState t) <- liftIO $ newTVarIO $ VSIdle
    let
        threadDo :: IO ()
        threadDo = do
            maction <-
                atomically $ do
                    vs <- readTVar bufferVar
                    case vs of
                        VSEnd -> do
                            writeTVar bufferVar VSIdle
                            return Nothing
                        VSException _ -> return Nothing
                        VSIdle -> mzero
                        VSRunning -> do
                            writeTVar bufferVar VSIdle
                            return $ Just $ return ()
                        VSPending vals True
                            | mus > 0 -> do
                                writeTVar bufferVar $ VSPending vals False
                                return $ Just $ threadDelay mus
                        VSPending vals _ -> do
                            writeTVar bufferVar VSRunning
                            return $ Just $ doit vals
            case maction of
                Just action -> do
                    catchExc action $ \ex -> atomically $ writeTVar bufferVar $ VSException ex
                    threadDo
                Nothing -> return ()
        waitForIdle :: STM (Result SomeException ())
        waitForIdle = do
            vs <- readTVar bufferVar
            case vs of
                VSIdle -> return $ return ()
                VSException ex -> return $ throwExc ex
                _ -> mzero
        atomicallyDo :: STM (Result SomeException a) -> IO a
        atomicallyDo stra = do
            ra <- atomically stra
            fromResultExc ra
        pushVal :: Maybe t -> IO ()
        pushVal (Just val) =
            atomicallyDo $ do
                vs <- readTVar bufferVar
                case vs of
                    VSEnd -> return $ return ()
                    VSException ex -> return $ throwExc ex
                    VSIdle -> do
                        writeTVar bufferVar $ VSPending val True
                        return $ return ()
                    VSRunning -> do
                        writeTVar bufferVar $ VSPending val True
                        return $ return ()
                    VSPending oldval _ -> do
                        writeTVar bufferVar $ VSPending (oldval <> val) True
                        return $ return ()
        pushVal Nothing =
            atomically $ do
                vs <- readTVar bufferVar
                case vs of
                    VSPending oldval False -> writeTVar bufferVar $ VSPending oldval True
                    _ -> return ()
        utask :: Task IO ()
        utask = let
            taskWait = atomicallyDo waitForIdle
            taskIsDone =
                atomicallyDo $ do
                    vs <- readTVar bufferVar
                    return $
                        case vs of
                            VSIdle -> return True
                            VSException _ -> return True
                            _ -> return False
            in MkTask {..}
    _ <- liftIO $ forkIO threadDo
    lifecycleOnClose $ do
        atomicallyDo $ do
            me <- waitForIdle
            case me of
                SuccessResult _ -> writeTVar bufferVar VSEnd
                FailureResult _ -> return ()
            return me
        atomicallyDo waitForIdle
    return (pushVal, utask)

asyncWorkerRunner ::
       forall t. Semigroup t
    => Text
    -> (t -> IO ())
    -> Lifecycle (t -> IO (), Task IO ())
asyncWorkerRunner name doit = do
    (asyncDoIt, utask) <- asyncWaitRunner name 0 doit
    return (\t -> asyncDoIt $ Just t, utask)

asyncTaskRunner ::
       forall t. Semigroup t
    => Text
    -> (t -> IO ())
    -> Lifecycle (t -> IO (), Task IO ())
asyncTaskRunner _ doit = do
    var <- liftIO $ newMVar mempty
    let
        pushVal :: t -> IO ()
        pushVal t =
            mVarRunStateT var $ do
                oldTask <- get
                newTask <-
                    lift $
                    forkTask $ do
                        taskWait oldTask
                        doit t
                put newTask
        utask :: Task IO ()
        utask = ioTask $ mVarRunStateT var get
    lifecycleOnClose $ taskWait utask
    return (pushVal, utask)

asyncRunnerINTERNAL :: Bool
asyncRunnerINTERNAL = False

asyncRunner ::
       forall t. Semigroup t
    => Text
    -> (t -> IO ())
    -> Lifecycle (t -> IO (), Task IO ())
asyncRunner =
    if asyncRunnerINTERNAL
        then asyncTaskRunner
        else asyncWorkerRunner

asyncIORunner :: Text -> Lifecycle (IO () -> IO (), Task IO ())
asyncIORunner name = do
    (asyncDoIt, utask) <- asyncRunner name sequence_
    return (\io -> asyncDoIt [io], utask)
