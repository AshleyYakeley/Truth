module Main
    ( main
    ) where

import Control.Applicative
import Control.Monad.Ology
import Control.Monad.Trans.Class
import Data.IORef
import Prelude
import Test.Tasty
import Test.Tasty.HUnit

testComposeInnerApplicative :: TestTree
testComposeInnerApplicative =
    testCase "Applicative" $ do
        r1 <- newIORef False
        r2 <- newIORef False
        let
            c1 :: ComposeInner Maybe IO ()
            c1 = do
                lift $ writeIORef r1 True
                liftInner Nothing
            c2 :: ComposeInner Maybe IO ()
            c2 = lift $ writeIORef r2 True
        _ <- getComposeInner $ liftA2 (,) c1 c2
        v1 <- readIORef r1
        v2 <- readIORef r2
        assertEqual "v1" True v1
        assertEqual "v2" False v2

testComposeInnerAlternative :: TestTree
testComposeInnerAlternative =
    testCase "Alternative" $ do
        r1 <- newIORef False
        r2 <- newIORef False
        let
            c1 :: ComposeInner Maybe IO ()
            c1 = lift $ writeIORef r1 True
            c2 :: ComposeInner Maybe IO ()
            c2 = lift $ writeIORef r2 True
        _ <- getComposeInner $ c1 <|> c2
        v1 <- readIORef r1
        v2 <- readIORef r2
        assertEqual "v1" True v1
        assertEqual "v2" False v2

testComposeInner :: TestTree
testComposeInner = testGroup "composeInner" [testComposeInnerApplicative, testComposeInnerAlternative]

tests :: TestTree
tests = testGroup "monadology" [testComposeInner]

main :: IO ()
main = defaultMain tests
