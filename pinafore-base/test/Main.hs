module Main
    ( main
    ) where

import Shapes
import Test.Anchor
import Test.Numeric
import Test.Storage
import Test.Tasty

tests :: TestTree
tests = testGroup "pinafore-base" [testNumeric, testAnchor, testStorage]

main :: IO ()
main = defaultMain tests
