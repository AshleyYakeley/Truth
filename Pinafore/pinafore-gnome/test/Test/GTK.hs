module Test.GTK
    ( getTestGTK
    ) where

import Changes.Core
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
           runLifecycle $
           runNewView $ do
               (pc, _) <- viewLiftLifecycle $ makeTestQContext hout
               action <- runWithContext pc (libraryFetchModule gnomeLibrary) $ qInterpretFile inpath
               action
               return ()

getTestGTK :: IO TestTree
getTestGTK = do
    return $ testTree "file" [testFile ("test" </> "gtk" </> "output.in"), testFile ("test" </> "gtk" </> "window.in")]
