{-# OPTIONS -fno-warn-orphans #-}

module Main
    ( main
    ) where

import Shapes
import Test.Entity ()
import Test.Language
import Test.ReadType
import Test.Tasty
import Test.Type

tests :: TestTree
tests = testGroup "pinafore" [testType, testLanguage, testReadTypes] {-, testEntity-}

main :: IO ()
main = defaultMain tests
