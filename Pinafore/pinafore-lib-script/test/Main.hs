module Main
    ( main
    ) where

import qualified Paths_pinafore_lib_script
import Pinafore.Library.GNOME
import Pinafore.Library.Media
import Pinafore.Test
import Shapes
import Shapes.Test
import System.Directory

testCheckModule :: String -> TestTree
testCheckModule name =
    testTree name $ do
        scriptLibDir <- Paths_pinafore_lib_script.getDataDir
        runTester defaultTester $
            testerLoadLibrary (mediaLibrary <> gnomeLibrary) $
            testerLoad (directoryLoadModule scriptLibDir) $ do
                mm <- testerLiftInterpreter $ runLoadModule (ibLoadModule ?behaviour) $ fromString name
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
    scriptLibDir <- Paths_pinafore_lib_script.getDataDir
    paths <- getRelFilePaths scriptLibDir
    return $ testTree "library" $ mapMaybe testRelPath paths

main :: IO ()
main = do
    testLibraries <- getTestLibraries
    let
        tests :: TestTree
        tests = testTree "pinafore-lib-script" [testLibraries]
    testMainNoSignalHandler tests
