module Test.Scripts
    ( getTestLibraries
    , testScripts
    ) where

import Pinafore
import Pinafore.Language.Library.GTK
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
    withTestPinaforeContext (libraryFetchModule gtkLibrary <> directoryFetchModule libDir) stdout $ \_ _ _getTableState -> do
        _ <- pinaforeInterpretFile fpath
        return ()

testCheckModule :: String -> TestTree
testCheckModule name =
    testTree name $
    withTestPinaforeContext (libraryFetchModule gtkLibrary <> directoryFetchModule libDir) stdout $ \_ _ _ -> do
        modname <- maybeToM "bad module name" $ toModuleName $ pack name
        _ <- throwInterpretResult $ runPinaforeScoped $ lcLoadModule ?library modname
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
              ["empty", "simple", "testquery", "test", "big-ui", "calendar", "drawing"]
        , testTree "example" $
          fmap
              (\name -> testCheckScript ("examples" </> name) name)
              ["stopwatch", "calendar", "contacts", "events", "clock", "fake-theme-system-journal"]
        ]
