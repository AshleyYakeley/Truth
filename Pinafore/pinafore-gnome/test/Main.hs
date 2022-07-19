module Main
    ( main
    ) where

import Shapes
import Shapes.Test
import Test.GTK
import Test.UI

main :: IO ()
main = do
    testGTK <- getTestGTK
    let
        tests :: TestTree
        tests = testTree "pinafore-gnome" [testGTK, testUI]
    testMainNoSignalHandler tests
