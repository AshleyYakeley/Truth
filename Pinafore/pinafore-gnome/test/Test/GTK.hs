module Test.GTK
    ( getTestGTK
    ) where

import Pinafore
import Pinafore.Language.Library.GNOME
import Pinafore.Test
import Shapes hiding ((.))
import Shapes.Test
import System.FilePath

testFile :: FilePath -> TestTree
testFile inpath = let
    dir = takeDirectory inpath
    testName = takeBaseName inpath
    in testHandleVsFile dir testName $ \hout ->
           runTester defaultTester {tstFetchModule = libraryFetchModule gnomeLibrary, tstOutput = hout} $ do
               testerLiftView $ do
                   action <- qInterpretFile inpath
                   action

getTestGTK :: IO TestTree
getTestGTK = do
    return $ testTree "file" [testFile ("test" </> "gtk" </> "output.in"), testFile ("test" </> "gtk" </> "window.in")]
