module Main
    ( main
    )
where

import Pinafore.Library.GNOME
import Pinafore.Library.Media
import Pinafore.Test
import Shapes
import Shapes.Test
import System.Directory

import Pinafore.Library.Script

testCheckModule :: String -> TestTree
testCheckModule name =
    testTree name $ do
        runTester defaultTester
            $ testerLoadPackages (mediaPackages <> gnomePackages <> scriptPackages)
            $ do
                mm <- testerLiftInterpreter $ runLoadModule (lcLoadModule ?library) $ fromString name
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
                    return
                        $ if isFile
                            then [e]
                            else []
    return $ mconcat ff

getTestLibraries :: IO TestTree
getTestLibraries = do
    pathss <- for (packageIncludeDirs scriptPackages) getRelFilePaths
    return $ testTree "library" $ mapMaybe testRelPath $ mconcat pathss

main :: IO ()
main = do
    testLibraries <- getTestLibraries
    let
        tests :: TestTree
        tests = testTree "pinafore-lib-script" [testLibraries]
    testMainNoSignalHandler tests
