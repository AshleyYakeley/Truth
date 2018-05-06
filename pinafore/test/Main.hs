{-# OPTIONS -fno-warn-orphans #-}

module Main
    ( main
    ) where

import Shapes
import Test.Language
import Test.Point
import Test.Tasty

tests :: TestTree
tests = testGroup "pinafore" [testLanguage, testPoint]

main :: IO ()
main = defaultMain tests
