module Test.Useful where

import Shapes
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden

goldenTest :: FilePath -> TestName -> ((?handle :: Handle) => IO ()) -> TestTree
goldenTest dirName testName call = let
    dir = "test/golden" </> dirName
    refPath = dir </> testName <> ".ref"
    outPath = dir </> testName <> ".out"
    in goldenVsFile testName refPath outPath $ do
           createDirectoryIfMissing True dir
           withBinaryFile outPath WriteMode $ \h -> let
               ?handle = h
               in do
                      hSetBuffering ?handle NoBuffering
                      call
