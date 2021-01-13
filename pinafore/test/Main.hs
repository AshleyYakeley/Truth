module Main
    ( main
    ) where

import Shapes
import Shapes.Test
import Test.Entity
import Test.GTK
import Test.Interactive
import Test.Language
import Test.Module
import Test.Output
import Test.ReadType
import Test.Scripts
import Test.Type
import Test.UI

main :: IO ()
main = do
    testOutput <- getTestOutput
    testGTK <- getTestGTK
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
                , testGTK
                , testModule
                , testInteractive
                , testUI
                ]
    testMainNoSignalHandler tests
