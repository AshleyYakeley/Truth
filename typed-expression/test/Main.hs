module Main
    ( main
    ) where

import Recursive
import Shapes
import Shim
import Test.Tasty

main :: IO ()
main = do
    let
        tests :: TestTree
        tests = testGroup "typed-expression" [testShim, testRecursive]
    defaultMain tests
