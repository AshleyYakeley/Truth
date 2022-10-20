module Test.Output
    ( getTestOutput
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
    in testHandleVsFile dir testName $ \hout ->
           runTester defaultTester {tstOutput = hout} $
           testerLiftView $ do
               action <- qInterpretFile inpath
               action

getTestOutput :: IO TestTree
getTestOutput = do
    inpaths <- findByExtension [".in"] $ "test" </> "output"
    return $ testTree "output" $ fmap testFile inpaths
