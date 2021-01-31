module Main
    ( main
    ) where

import Shapes
import Shapes.Test
import Test.Scripts

main :: IO ()
main = do
    let
        tests :: TestTree
        tests = testTree "pinafore" [testScripts]
    testMainNoSignalHandler tests
