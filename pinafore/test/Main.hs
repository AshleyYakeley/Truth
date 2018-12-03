{-# OPTIONS -fno-warn-orphans #-}

module Main
    ( main
    ) where

import Shapes
import Test.Entity
import Test.Examples
import Test.Language
import Test.ReadType
import Test.Tasty
import Test.Type

tests :: TestTree
tests = testGroup "pinafore" [testType, testLanguage, testReadTypes, testEntity, testExamples]

main :: IO ()
main = defaultMain tests
