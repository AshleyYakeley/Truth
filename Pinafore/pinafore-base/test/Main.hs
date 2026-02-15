module Main
    ( main
    )
where

import Shapes
import Shapes.Test

import Test.Anchor
import Test.Numeric

tests :: TestTree
tests = testTree "pinafore-base" [testNumeric, testAnchor]

main :: IO ()
main = testMain tests
