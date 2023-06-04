module Main
    ( main
    ) where

import Shapes
import Shapes.Test
import Test.Golden
import Test.UI

main :: IO ()
main = do
    let
        tests :: TestTree
        tests = testTree "pinafore-gnome" [testGolden, testUI]
    testMainNoSignalHandler tests
