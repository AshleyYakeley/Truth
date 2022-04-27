module LifeCycle
    ( testLifeCycle
    ) where

import Control.Monad.Ology
import Prelude
import Test.Tasty
import Test.Tasty.HUnit
import Useful

testLifeCycleRun :: TestTree
testLifeCycleRun =
    testCase "run" $
    compareTest "ACDB" $ \appendStr -> do
        let
            lc :: LifeCycle ()
            lc = do
                liftIO $ appendStr "A"
                lifeCycleOnClose $ appendStr "B"
                liftIO $ appendStr "C"
                lifeCycleOnClose $ appendStr "D"
        runLifeCycleT lc

testLifeCycleWith :: TestTree
testLifeCycleWith =
    testCase "with" $
    compareTest "ABECFD" $ \appendStr -> do
        let
            lc :: LifeCycle ()
            lc = do
                liftIO $ appendStr "A"
                s <-
                    lifeCycleWith $ \call -> do
                        appendStr "B"
                        v <- call "C"
                        appendStr "D"
                        return v
                liftIO $ appendStr "E"
                liftIO $ appendStr s
                liftIO $ appendStr "F"
        runLifeCycleT lc

testLifeCycle :: TestTree
testLifeCycle = testGroup "lifecycle" [testLifeCycleRun, testLifeCycleWith]
