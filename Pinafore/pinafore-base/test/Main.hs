module Main
    ( main
    )
where

import Shapes
import Shapes.Test

import Test.Anchor
import Test.Numeric
import Test.ReadShow
import Test.Storage

tests :: TestTree
tests = testTree "pinafore-base" [testNumeric, testAnchor, testStorage, testReadShow]

main :: IO ()
main = testMain tests
