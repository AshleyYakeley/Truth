module Debug.ThreadTrace where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class
import qualified Data.Map as Map
import Data.Time.Clock.System
import Data.Word
import Debug.Trace
import GHC.Conc
import Prelude
import System.IO.Unsafe

traceThreadNames :: MVar (Map.Map ThreadId String)
{-# NOINLINE traceThreadNames #-}
traceThreadNames = unsafePerformIO $ newMVar mempty

contextStr :: String -> String -> String
contextStr "" b = b
contextStr a b = a ++ ": " ++ b

traceNameThread :: String -> IO ()
traceNameThread name = do
    tid <- myThreadId
    modifyMVar_ traceThreadNames $ \m -> return $ Map.insert tid name m

traceIOM :: MonadIO m => String -> m ()
traceIOM msg =
    liftIO $ do
        MkSystemTime s ns <- getSystemTime
        th <- myThreadId
        names <- readMVar traceThreadNames
        let
            nametxt =
                case Map.lookup th names of
                    Just name -> " (" ++ name ++ ")"
                    Nothing -> ""
            showMod :: Int -> Word32 -> String
            showMod 0 _ = ""
            showMod n x = showMod (pred n) (div x 10) <> show (mod x 10)
        traceIO $ show s <> "." <> showMod 9 ns <> ": " <> show th <> nametxt <> ": " <> msg

traceBracketArgs :: MonadIO m => String -> String -> (r -> String) -> m r -> m r
traceBracketArgs s args showr ma = do
    traceIOM $
        s ++
        " [" ++
        (if null args
             then ""
             else " " ++ args)
    a <- ma
    let ret = showr a
    traceIOM $
        s ++
        " ]" ++
        (if null ret
             then ""
             else " => " ++ ret)
    return a

traceBracket :: MonadIO m => String -> m r -> m r
traceBracket s = traceBracketArgs s "" (\_ -> "")

traceBracketIO :: String -> IO r -> IO r
traceBracketIO s ma = do
    traceIOM $ s ++ " ["
    catch
        (do
             a <- ma
             traceIOM $ s ++ " ]"
             return a) $ \(e :: SomeException) -> do
        traceIOM $ s ++ " ! " ++ show e
        throw e

traceThread :: String -> IO r -> IO r
traceThread name ma = do
    traceNameThread name
    traceBracket "THREAD" ma

traceForkIO :: String -> IO () -> IO ThreadId
traceForkIO name ma = traceBracketArgs "FORK" "" show $ forkIO $ traceThread name ma

traceEvaluate :: MonadIO m => String -> r -> m r
traceEvaluate s a = liftIO $ traceBracketIO ("evaluate " <> s) $ evaluate a

traceBarrier :: (MonadIO m1, MonadIO m2) => String -> (m1 a -> m2 b) -> m1 a -> m2 b
traceBarrier s tr ma = traceBracket (contextStr s "outside") $ tr $ traceBracket (contextStr s "inside") ma

traceBarrierIO :: String -> (IO a -> IO b) -> IO a -> IO b
traceBarrierIO s tr ma = traceBracketIO (contextStr s "outside") $ tr $ traceBracketIO (contextStr s "inside") ma

tracePure :: String -> a -> a
tracePure s = seq (unsafePerformIO (traceIOM s))

tracePureBracket :: Monad m => String -> m a -> m a
tracePureBracket s ma = (tracePure (s ++ " [") ma) >>= (\a -> return $ tracePure (s ++ " ]") a)

class TraceThing t where
    traceThing :: String -> t -> t

instance {-# OVERLAPPABLE #-} MonadIO m => TraceThing (m a) where
    traceThing s ma = traceBracket s ma

instance TraceThing t => TraceThing (a -> t) where
    traceThing s at a = traceThing s $ at a

traceSTM :: String -> STM ()
traceSTM msg = unsafeIOToSTM $ traceIOM msg

traceBracketSTM :: String -> STM a -> STM a
traceBracketSTM s ma = do
    traceSTM $ s ++ " ["
    a <-
        ma <|> do
            traceSTM $ s ++ " ] RETRY"
            empty
    traceSTM $ s ++ " ]"
    return a
