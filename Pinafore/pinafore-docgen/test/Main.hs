module Main
    ( main
    ) where

import Pinafore.DocGen
import Pinafore.Language
import Pinafore.Main
import Pinafore.Options
import Shapes
import Shapes.Test
import System.FilePath

testFile :: ModuleOptions -> FilePath -> TestTree
testFile mo inpath = let
    dir = takeDirectory inpath
    modName = takeBaseName inpath
    in testHandleVsFileInDir dir modName $ \outh -> do generateCommonMarkDoc outh mo $ MkModuleName $ pack modName

getTestPaths :: IO [FilePath]
getTestPaths = do
    inpaths <- findByExtension [".pinafore"] $ "test" </> "golden"
    case inpaths of
        [] -> fail "wrong directory"
        _ -> return ()
    return inpaths

main :: IO ()
main = do
    inpaths <- getTestPaths
    let
        roCache = False
        roIncludeDirs = ["test" </> "golden"]
        roDataDir = Nothing
    mo <- getModelOptions MkRunOptions {..}
    let
        testGolden :: TestTree
        testGolden = testTree "golden" $ fmap (testFile mo) inpaths
        tests :: TestTree
        tests = testTree "pinafore-docgen" [testGolden]
    testMainNoSignalHandler tests
