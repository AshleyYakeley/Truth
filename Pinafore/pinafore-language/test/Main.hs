module Main
    ( main
    ) where

import Shapes
import Shapes.Test
import Test.Entity
import Test.Interactive
import Test.Language
import Test.Module
import Test.Output
import Test.ReadType
import Test.Type
import Test.Unifier

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
                , testUnifier
                , testEntity
                , testUpdates
                , testOutput
                , testModule
                , testInteractive
                ]
    testMainNoSignalHandler tests
