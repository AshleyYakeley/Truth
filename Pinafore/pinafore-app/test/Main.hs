module Main
    ( main
    ) where

import Shapes
import Shapes.Test
import Test.Interactive
import Test.Library
import Test.Scripts

main :: IO ()
main = do
    testInteractive <- getTestInteractive
    testScripts <- getTestScripts
    let
        tests :: TestTree
        tests = testTree "pinafore-app" [testInteractive, testScripts, testLibrary]
    testMainNoSignalHandler tests
