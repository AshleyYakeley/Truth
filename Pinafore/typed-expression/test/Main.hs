module Main
    ( main
    )
where

import Shapes
import Shapes.Test

import Shim

tests :: TestTree
tests = testTree "typed-expression" [testShim]

main :: IO ()
main = testMain tests
