module Main
    ( main
    ) where

import Flags
import GIO
import Lock
import Matrix
import Shapes
import Shapes.Test

tests :: [TestTree]
tests = [matrixTest, gioTests] <> ifpure flag_TestX11 lockTests

main :: IO ()
main = testMain $ testTree "changes-gnome" tests
