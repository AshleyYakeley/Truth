module Main
    ( main
    ) where

import Compose
import Coroutine
import Exception
import LifeCycle
import Prelude
import Test.Tasty

tests :: TestTree
tests = testGroup "monadology" [testCoroutine, testLifeCycle, testComposeInner, testException]

main :: IO ()
main = defaultMain tests
