module Main
    ( main
    )
where

import Shapes
import Shapes.Test

import Flags
import Test.Golden
import Test.UI

tests :: [TestTree]
tests = [testGolden] <> ifpure flag_TestX11 testUI

main :: IO ()
main = testMainNoSignalHandler $ testTree "gnome" tests
