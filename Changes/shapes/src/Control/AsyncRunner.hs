module Control.AsyncRunner
    ( asyncWaitRunner
    , asyncRunner
    , asyncIORunner
    ) where

import Control.Monad.LifeCycle
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
    => Int
    -> (t -> IO ())
    -> LifeCycle (Maybe t -> IO (), Task ())
asyncWaitRunner mus doit = do
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
        utask :: Task ()
        utask = let
            taskWait = atomicallyDo waitForIdle
            taskIsDone =
                atomicallyDo $ do
                    vs <- readTVar bufferVar
                    return $
                        case vs of
                            VSIdle -> return $ Just ()
                            VSException ex -> throwExc ex
                            _ -> return Nothing
            in MkTask {..}
    _ <- liftIO $ forkIO threadDo
    lifeCycleClose $ do
        atomicallyDo $ do
            me <- waitForIdle
            case me of
                SuccessResult _ -> writeTVar bufferVar VSEnd
                FailureResult _ -> return ()
            return me
        atomicallyDo waitForIdle
    return (pushVal, utask)

asyncRunner ::
       forall t. Semigroup t
    => (t -> IO ())
    -> LifeCycle (t -> IO (), Task ())
asyncRunner doit = do
    (asyncDoIt, utask) <- asyncWaitRunner 0 doit
    return (\t -> asyncDoIt $ Just t, utask)

asyncIORunner :: LifeCycle (IO () -> IO (), Task ())
asyncIORunner = do
    (asyncDoIt, utask) <- asyncRunner sequence_
    return (\io -> asyncDoIt [io], utask)
