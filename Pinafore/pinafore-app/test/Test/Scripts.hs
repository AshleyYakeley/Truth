module Test.Scripts
    ( getTestLibraries
    , testScripts
    ) where

import Pinafore
import Pinafore.Libs
import Pinafore.Test
import Shapes
import Shapes.Test
import System.Directory

libDir :: FilePath
libDir = "../lib"

-- Just check, don't run
testCheckScript :: FilePath -> String -> TestTree
testCheckScript fpath name =
    testTree name $
    runTester defaultTester {tstFetchModule = libraryFetchModule extraLibrary <> directoryFetchModule libDir} $ do
        _ <- testerLiftView $ qInterpretFile fpath
        return ()

testCheckModule :: String -> TestTree
testCheckModule name =
    testTree name $
    runTester defaultTester {tstFetchModule = libraryFetchModule extraLibrary <> directoryFetchModule libDir} $ do
        _ <- testerLiftInterpreter $ lcLoadModule ?library $ fromString name
        return ()

testRelPath :: FilePath -> Maybe TestTree
testRelPath relpath = do
    path <- endsWith ".pinafore" relpath
    return $
        testCheckModule $
        fmap
            (\c ->
                 if c == '/'
                     then '.'
                     else c)
            path

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
    paths <- getRelFilePaths libDir
    return $ testTree "library" $ mapMaybe testRelPath paths

testScripts :: TestTree
testScripts =
    testTree
        "scripts"
        [ testTree "test" $
          fmap
              (\name -> testCheckScript ("test/script" </> name) name)
              ["empty", "simple", "testquery", "test", "big-ui", "calendar", "drawing", "choose"]
        , testTree "example" $
          fmap
              (\name -> testCheckScript ("examples" </> name) name)
              ["stopwatch", "calendar", "contacts", "events", "clock", "fake-theme-system-journal"]
        ]
