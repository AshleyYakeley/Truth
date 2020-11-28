module Test.Useful where

import Shapes
import Shapes.Test

goldenTest :: FilePath -> TestName -> ((?handle :: Handle) => IO ()) -> TestTree
goldenTest dirName testName call = let
    dir = "test/golden" </> dirName
    in testHandleVsFile dir testName $ \h -> let
           ?handle = h
           in call
