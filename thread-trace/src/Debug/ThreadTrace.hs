module Debug.ThreadTrace where

import Data.Time.Clock.System
import Debug.Trace
import System.IO.Unsafe
import Prelude
import Data.Word
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Exception

contextStr :: String -> String -> String
contextStr "" b = b
contextStr a b = a ++ ": " ++ b

traceIOM :: MonadIO m => String -> m ()
traceIOM msg =
    liftIO $ do
        MkSystemTime s ns <- getSystemTime
        th <- myThreadId
        let
            showMod :: Int -> Word32 -> String
            showMod 0 _ = ""
            showMod n x = showMod (pred n) (div x 10) <> show (mod x 10)
        traceIO $ show s <> "." <> showMod 9 ns <> ": " <> show th <> ": " <> msg

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
    catch (do
        a <- ma
        traceIOM $ s ++ " ]"
        return a
        )
        $ \(e :: SomeException) -> do
            traceIOM $ s ++ " ! " ++ show e
            throw e

traceBarrier :: (MonadIO m1,MonadIO m2) => String -> (m1 a -> m2 b) -> m1 a -> m2 b
traceBarrier s tr ma = traceBracket (contextStr s "outside") $ tr $ traceBracket (contextStr s  "inside") ma

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
