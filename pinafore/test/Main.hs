module Main
    ( main
    ) where

import Shapes
import Shapes.Test
import Test.Entity
import Test.Interactive
import Test.Language
import Test.ReadType
import Test.Scripts
import Test.Type
import Test.UI

main :: IO ()
main = do
    testInteractive <- getTestInteractive
    let
        tests :: TestTree
        tests =
            testTree
                "pinafore"
                [testType, testLanguage, testReadTypes, testEntity, testUpdates, testScripts, testInteractive, testUI]
    testMain tests
