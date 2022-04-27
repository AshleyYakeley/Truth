module Useful where

import Data.IORef
import Prelude
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

withMessage :: Monad m => (String -> m ()) -> String -> m r -> m r
withMessage appendStr s m = do
    appendStr $ "+" <> s
    r <- m
    appendStr $ "-" <> s
    return r
