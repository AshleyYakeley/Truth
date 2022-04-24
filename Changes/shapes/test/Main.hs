module Main
    ( main
    ) where

import Data.IORef
import Data.Time
import Shapes
import Shapes.Test

compareTest :: String -> ((String -> IO ()) -> IO r) -> IO r
compareTest expected action = do
    resultsRef <- newIORef ""
    let
        appendStr :: String -> IO ()
        appendStr s = modifyIORef resultsRef $ \t -> t ++ s
    r <- action appendStr
    found <- readIORef resultsRef
    assertEqual "" expected found
    return r

withMessage :: (String -> IO ()) -> String -> IO r -> IO r
withMessage appendStr s m = do
    appendStr $ "+" <> s
    r <- m
    appendStr $ "-" <> s
    return r

testCoroutine :: TestTree
testCoroutine =
    testTree "coroutine" $
    compareTest "+A+B-A-B" $ \appendStr -> do
        _ <-
            coroutine
                (\y ->
                     withMessage appendStr "A" $ do
                         y ()
                         return ((), ()))
                (\() y ->
                     withMessage appendStr "B" $ do
                         y ()
                         return ((), ()))
        return ()

testLifeCycle :: TestTree
testLifeCycle =
    testTree "lifecycle" $
    compareTest "ACDB" $ \appendStr -> do
        let
            lc :: LifeCycle ()
            lc = do
                liftIO $ appendStr "A"
                lifeCycleOnClose $ appendStr "B"
                liftIO $ appendStr "C"
                lifeCycleOnClose $ appendStr "D"
        runLifeCycleT lc

baseTime :: UTCTime
baseTime = UTCTime (ModifiedJulianDay 0) 0

testFastClock :: TestTree
testFastClock =
    testTree "fast" $ do
        ref <- newIORef False
        runLifeCycleT $ do
            _ <-
                clock baseTime 0.1 $ \_ -> do
                    writeIORef ref True
                    threadDelay 1000000
                    writeIORef ref False
            liftIO $ threadDelay 500000
        bad <- readIORef ref
        if bad
            then assertFailure "bad async exception"
            else return ()

testSlowClock :: TestTree
testSlowClock =
    testTree "slow" $
    runLifeCycleT $ do
        _ <- clock baseTime (5000 * nominalDay) $ \_ -> return ()
        return ()

testClock :: TestTree
testClock = testTree "clock" [testFastClock, testSlowClock]

tests :: TestTree
tests = testTree "shapes" [testCoroutine, testLifeCycle, testClock]

main :: IO ()
main = testMain tests
