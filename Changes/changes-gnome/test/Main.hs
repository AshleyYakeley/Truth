module Main
    ( main
    )
where

import Shapes
import Shapes.Test

import Flags
import GIO
import Lock
import Matrix

tests :: [TestTree]
tests = [matrixTest, gioTests] <> ifpure flag_TestX11 lockTests

main :: IO ()
main = testMain $ testTree "changes-gnome" tests
