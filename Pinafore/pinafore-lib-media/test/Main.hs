module Main
    ( main
    )
where

import Shapes
import Shapes.Test

import Test.Golden

tests :: [TestTree]
tests = [testGolden]

main :: IO ()
main = testMainNoSignalHandler $ testTree "media" tests
