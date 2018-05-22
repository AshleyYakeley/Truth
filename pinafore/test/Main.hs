{-# OPTIONS -fno-warn-orphans #-}

module Main
    ( main
    ) where

import Shapes
import Test.Entity
import Test.Language
import Test.Tasty

tests :: TestTree
tests = testGroup "pinafore" [testLanguage, testEntity]

main :: IO ()
main = defaultMain tests
