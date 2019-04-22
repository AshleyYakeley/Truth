module Main where

import Data.IORef
import Shapes
import Test.Tasty
import Test.Tasty.HUnit

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
    testCase "coroutine" $
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
    testCase "lifecycle" $
    compareTest "ACDB" $ \appendStr -> do
        let
            lc :: LifeCycleIO ()
            lc = do
                liftIO $ appendStr "A"
                lifeCycleClose $ appendStr "B"
                liftIO $ appendStr "C"
                lifeCycleClose $ appendStr "D"
        runLifeCycle lc

tests :: TestTree
tests = testGroup "shapes" [testCoroutine, testLifeCycle]

main :: IO ()
main = defaultMain tests
