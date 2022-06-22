module Test.GTK
    ( getTestGTK
    ) where

import Changes.Core
import Pinafore
import Pinafore.Language.Library.GTK
import Pinafore.Test
import Shapes hiding ((.))
import Shapes.Test
import System.FilePath

testFile :: FilePath -> TestTree
testFile inpath = let
    dir = takeDirectory inpath
    testName = takeBaseName inpath
    in testHandleVsFile dir testName $ \hout ->
           runLifeCycleT $
           runNewView $ do
               pc <-
                   viewLiftLifeCycle $ do
                       (model, _) <- makeTestStorageModel
                       makePinaforeContext nullInvocationInfo hout model
               action <- runWithContext pc (libraryFetchModule gtkLibrary) $ pinaforeInterpretFile inpath
               action
               return ()

getTestGTK :: IO TestTree
getTestGTK = do
    return $ testTree "file" [testFile ("test" </> "gtk" </> "output.in"), testFile ("test" </> "gtk" </> "window.in")]
