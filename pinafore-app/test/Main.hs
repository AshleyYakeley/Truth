module Main
    ( main
    ) where

import Shapes
import Shapes.Test
import Test.Interactive
import Test.Scripts

main :: IO ()
main = do
    testLibraries <- getTestLibraries
    testInteractive <- getTestInteractive
    let
        tests :: TestTree
        tests = testTree "pinafore" [testLibraries, testInteractive, testScripts]
    testMainNoSignalHandler tests
