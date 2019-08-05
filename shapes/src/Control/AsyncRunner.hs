module Control.AsyncRunner
    ( asyncWaitRunner
    , asyncRunner
    , asyncIORunner
    ) where

import Control.Monad.LifeCycleIO
import Control.Monad.Trans.LifeCycle
import Shapes.Import

data VarState t
    = VSEmpty
    | VSDo t
           Bool
    | VSDone

asyncWaitRunner ::
       forall t. Semigroup t
    => Int
    -> (t -> IO ())
    -> LifeCycleIO (Maybe t -> IO ())
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
    -> LifeCycleIO (t -> IO ())
asyncRunner doit = fmap (\push -> push . Just) $ asyncWaitRunner 0 doit

asyncIORunner :: LifeCycleIO (IO () -> IO ())
asyncIORunner = fmap (\pushVal io -> pushVal [io]) $ asyncRunner sequence_
