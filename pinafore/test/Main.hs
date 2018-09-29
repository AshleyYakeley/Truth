{-# OPTIONS -fno-warn-orphans #-}

module Main
    ( main
    ) where

import Shapes
import Test.Entity ()
import Test.Language
import Test.Tasty
import Test.Type

tests :: TestTree
tests = testGroup "pinafore" [testType, testLanguage] {-, testEntity-}

main :: IO ()
main = defaultMain tests
