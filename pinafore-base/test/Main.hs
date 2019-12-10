module Main
    ( main
    ) where

import Shapes
import Test.Numeric
import Test.Tasty

tests :: TestTree
tests = testGroup "pinafore-base" [testNumeric]

main :: IO ()
main = defaultMain tests
