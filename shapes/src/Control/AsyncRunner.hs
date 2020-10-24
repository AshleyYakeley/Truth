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
    | VSRunning
    | VSPending t
                Bool
    | VSEnd

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
                        VSIdle -> do mzero
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
                    action
                    threadDo
                Nothing -> return ()
        waitForIdle :: STM ()
        waitForIdle = do
            vs <- readTVar bufferVar
            case vs of
                VSIdle -> return ()
                _ -> mzero
        pushVal :: Maybe t -> IO ()
        pushVal (Just val) =
            atomically $ do
                vs <- readTVar bufferVar
                case vs of
                    VSEnd -> return ()
                    VSIdle -> writeTVar bufferVar $ VSPending val True
                    VSRunning -> writeTVar bufferVar $ VSPending val True
                    VSPending oldval _ -> writeTVar bufferVar $ VSPending (oldval <> val) True
        pushVal Nothing =
            atomically $ do
                vs <- readTVar bufferVar
                case vs of
                    VSPending oldval False -> writeTVar bufferVar $ VSPending oldval True
                    _ -> return ()
        utask :: Task ()
        utask = let
            taskWait = atomically waitForIdle
            taskIsDone =
                atomically $ do
                    vs <- readTVar bufferVar
                    return $
                        case vs of
                            VSIdle -> Just ()
                            _ -> Nothing
            in MkTask {..}
    _ <- liftIO $ forkIO threadDo
    lifeCycleClose $ do
        atomically $ do
            waitForIdle
            writeTVar bufferVar $ VSEnd
        atomically waitForIdle
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
