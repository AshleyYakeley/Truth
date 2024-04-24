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

getPlainTestPaths :: IO [FilePath]
getPlainTestPaths = do
    paths <- findByExtension [".pinafore"] $ "test" </> "golden"
    case paths of
        [] -> fail "wrong directory"
        _ -> return ()
    return paths

testPlainFile :: ModuleOptions -> FilePath -> TestTree
testPlainFile mo inpath = let
    dir = takeDirectory inpath
    modName = takeBaseName inpath
    in testHandleVsFileInDir dir modName $ \outh ->
           generateCommonMarkDoc outh mo $ PlainModuleSpec $ MkModuleName $ pack modName

getOpenAPITestPaths :: IO [FilePath]
getOpenAPITestPaths = do
    paths <- findByExtension [".json"] $ "test" </> "golden" </> "openapi"
    case paths of
        [] -> fail "wrong directory"
        _ -> return ()
    return paths

testOpenAPIFile :: ModuleOptions -> FilePath -> TestTree
testOpenAPIFile mo inpath = let
    dir = takeDirectory inpath
    testName = takeBaseName inpath
    in testHandleVsFileInDir dir testName $ \outh ->
           generateCommonMarkDoc outh mo $ SpecialModuleSpec "openapi" $ "file:" <> pack inpath

main :: IO ()
main = do
    plainpaths <- getPlainTestPaths
    openapipaths <- getOpenAPITestPaths
    let
        roCache = False
        roIncludeDirs = ["test" </> "golden"]
        roDataDir = Nothing
    mo <- getModelOptions MkRunOptions {..}
    let
        testPlain :: TestTree
        testPlain = testTree "plain" $ fmap (testPlainFile mo) plainpaths
        testOpenAPI :: TestTree
        testOpenAPI = testTree "openapi" $ fmap (testOpenAPIFile mo) openapipaths
        tests :: TestTree
        tests = testTree "pinafore-docgen" [testPlain, testOpenAPI]
    testMainNoSignalHandler tests
