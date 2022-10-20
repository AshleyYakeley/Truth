module Test.Interactive
    ( getTestInteractive
    ) where

import Pinafore
import Pinafore.Test
import Shapes hiding ((.))
import Shapes.Test
import System.FilePath

testFile :: FilePath -> TestTree
testFile inpath = let
    dir = takeDirectory inpath
    testName = takeBaseName inpath
    in testHandleVsFile dir testName $ \outh ->
           withBinaryFile inpath ReadMode $ \inh ->
               runTester defaultTester {tstOutput = outh} $ do
                   testerLiftView $ qInteractHandles inh outh True
                   liftIO $ hPutStrLn outh "<END>"

getTestInteractive :: IO TestTree
getTestInteractive = do
    inpaths <- findByExtension [".in"] $ "test" </> "interactive"
    return $ testTree "interactive" $ fmap testFile inpaths
