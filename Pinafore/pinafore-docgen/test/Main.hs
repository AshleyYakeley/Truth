module Main
    ( main
    )
where

import Pinafore.Documentation
import Pinafore.Options
import Shapes
import Shapes.Test
import System.FilePath

import Pinafore.DocGen

testFile :: ModuleOptions -> FilePath -> TestTree
testFile mo inpath = let
    dir = takeDirectory inpath
    modName = takeBaseName inpath
    in testHandleVsFileInDir dir modName $ \outh -> generateCommonMarkDoc outh mo $ MkModuleName $ pack modName

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
        roIncludeDirs = ["test" </> "golden"]
        roDataDir = Nothing
    mo <- getModuleOptions MkRunOptions{..}
    let
        testGolden :: TestTree
        testGolden = testTree "golden" $ fmap (testFile mo) inpaths
        tests :: TestTree
        tests = testTree "pinafore-docgen" [testGolden]
    testMainNoSignalHandler tests
