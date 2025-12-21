module Main
    ( main
    )
where

import Shapes
import Shapes.Test

import Test.Anchor
import Test.Numeric
import Test.ReadShow

tests :: TestTree
tests = testTree "pinafore-base" [testNumeric, testAnchor, testReadShow]

main :: IO ()
main = testMain tests
