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
import Test.Subtype
import Test.Token
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
                [ testToken
                , testType
                , testLanguage
                , testReadTypes
                , testUnifier
                , testSubtype
                , testEntity
                , testUpdates
                , testOutput
                , testNamespace
                , testModule
                , testInteractive
                ]
    testMainNoSignalHandler tests
