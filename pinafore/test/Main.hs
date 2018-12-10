{-# OPTIONS -fno-warn-orphans #-}

module Main
    ( main
    ) where

import Shapes
import Test.Entity
import Test.Examples
import Test.Interactive
import Test.Language
import Test.ReadType
import Test.Tasty
import Test.Type

main :: IO ()
main = do
    testInteractive <- getTestInteractive
    let
        tests :: TestTree
        tests = testGroup "pinafore" [testType, testLanguage, testReadTypes, testEntity, testExamples, testInteractive]
    defaultMain tests
