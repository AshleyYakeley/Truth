module Main where

import Data.IORef
import Shapes
import Test.Tasty
import Test.Tasty.HUnit

testLifeCycle :: TestTree
testLifeCycle =
    testCase "lifecycle" $ do
        resultsRef <- newIORef ""
        let
            appendStr :: String -> IO ()
            appendStr s = modifyIORef resultsRef $ \t -> t ++ s
            lc :: LifeCycle ()
            lc = do
                liftIO $ appendStr "A"
                lifeCycleClose $ appendStr "B"
                liftIO $ appendStr "C"
                lifeCycleClose $ appendStr "D"
        withLifeCycle lc return
        found <- readIORef resultsRef
        assertEqual "" "ACDB" found

tests :: TestTree
tests = testGroup "shapes" [testLifeCycle]

main :: IO ()
main = defaultMain tests
