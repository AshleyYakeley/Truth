module Test.GTK
    ( getTestGTK
    ) where

import Changes.UI.GTK
import Pinafore
import Pinafore.Language.Library.GTK
import Pinafore.Test
import Shapes hiding ((<.>))
import Shapes.Test
import System.FilePath
import Changes.Debug.Reference

testFile :: FilePath -> TestTree
testFile inpath = let
    dir = takeDirectory inpath
    testName = takeBaseName inpath
    in testHandleVsFile dir testName $ \hout ->
           traceBracketIO "testFile" $
           changesMainGTK $ \cc -> do
               pc <-
                   liftLifeCycle $ do
                       (model, _) <- makeTestStorageModel
                       makePinaforeContext nullInvocationInfo hout model cc
               action <- runWithContext pc (libraryFetchModule gtkLibrary) $ pinaforeInterpretFile inpath
               liftToLifeCycle action
               return ()

getTestGTK :: IO TestTree
getTestGTK = do
    return $ testTree "gtk" [testFile ("test" </> "gtk" </> "output.in"), testFile ("test" </> "gtk" </> "window.in")]
