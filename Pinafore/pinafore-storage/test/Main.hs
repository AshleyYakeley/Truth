module Main
    ( main
    )
where

import Shapes
import Shapes.Test

import Test.Storage

tests :: TestTree
tests = testTree "pinafore-storage" [testStorage]

main :: IO ()
main = testMain tests
