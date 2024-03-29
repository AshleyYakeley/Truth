module Main
    ( main
    ) where

import Paths_pinafore_stdlib
import Pinafore
import Pinafore.Language.Library.GNOME
import Pinafore.Language.Library.Media
import Pinafore.Test
import Shapes
import Shapes.Test
import System.Directory

extraLibrary :: [LibraryModule ()]
extraLibrary = mediaLibrary <> gnomeLibrary

testCheckModule :: String -> TestTree
testCheckModule name =
    testTree name $ do
        libDir <- getDataDir
        runTester defaultTester {tstFetchModule = libraryFetchModule extraLibrary <> directoryFetchModule libDir} $ do
            mm <- testerLiftInterpreter $ lcLoadModule ?library $ fromString name
            case mm of
                Just _ -> return ()
                Nothing -> fail "module not found"

testRelPath :: FilePath -> Maybe TestTree
testRelPath relpath = do
    path <- endsWith ".pinafore" relpath
    return $ testCheckModule path

getRelFilePaths :: FilePath -> IO [FilePath]
getRelFilePaths dir = do
    ee <- listDirectory dir
    ff <-
        for ee $ \e -> do
            let f = dir </> e
            isDir <- doesDirectoryExist f
            if isDir
                then do
                    subtree <- getRelFilePaths f
                    return $ fmap (\p -> e </> p) subtree
                else do
                    isFile <- doesFileExist f
                    return $
                        if isFile
                            then [e]
                            else []
    return $ mconcat ff

getTestLibraries :: IO TestTree
getTestLibraries = do
    libDir <- getDataDir
    paths <- getRelFilePaths libDir
    return $ testTree "library" $ mapMaybe testRelPath paths

main :: IO ()
main = do
    testLibraries <- getTestLibraries
    let
        tests :: TestTree
        tests = testTree "pinafore-stdlib" [testLibraries]
    testMainNoSignalHandler tests
