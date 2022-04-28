module Main
    ( main
    ) where

import Data.IORef
import Data.Time
import Shapes
import Shapes.Test

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
tests = testTree "shapes" [testClock]

main :: IO ()
main = testMain tests
