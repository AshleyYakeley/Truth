module Main
    ( main
    ) where

import Shapes
import Shapes.Test
import Test.Interactive
import Test.Scripts

main :: IO ()
main = do
    testInteractive <- getTestInteractive
    let
        tests :: TestTree
        tests = testTree "pinafore" [testInteractive, testScripts]
    testMainNoSignalHandler tests
