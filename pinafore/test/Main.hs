{-# OPTIONS -fno-warn-orphans #-}

module Main
    ( main
    ) where

import Shapes
import Test.Entity
import Test.Interactive
import Test.Language
import Test.ReadType
import Test.Scripts
import Test.Tasty
import Test.Type

main :: IO ()
main = do
    testInteractive <- getTestInteractive
    let
        tests :: TestTree
        tests = testGroup "pinafore" [testType, testLanguage, testReadTypes, testEntity, testScripts, testInteractive]
    defaultMain tests
