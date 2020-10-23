module Main
    ( main
    ) where

import Shapes
import Shapes.Test
import Test.Entity
import Test.Interactive
import Test.Language
import Test.Output
import Test.ReadType
import Test.Scripts
import Test.Type
import Test.UI

main :: IO ()
main = do
    testOutput <- getTestOutput
    testInteractive <- getTestInteractive
    let
        tests :: TestTree
        tests =
            testTree
                "pinafore"
                [ testType
                , testLanguage
                , testReadTypes
                , testEntity
                , testUpdates
                , testScripts
                , testOutput
                , testInteractive
                , testUI
                ]
    testMain tests
