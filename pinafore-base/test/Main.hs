module Main
    ( main
    ) where

import Shapes
import Test.Anchor
import Test.Numeric
import Test.Tasty

tests :: TestTree
tests = testGroup "pinafore-base" [testNumeric, testAnchor]

main :: IO ()
main = defaultMain tests
