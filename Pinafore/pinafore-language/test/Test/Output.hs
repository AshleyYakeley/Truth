module Test.Output
    ( getTestOutput
    ) where

import Pinafore.Test.Internal
import Shapes hiding ((.))
import Shapes.Test
import System.FilePath

testFile :: FilePath -> TestTree
testFile inpath = let
    dir = takeDirectory inpath
    testName = takeBaseName inpath
    modifier :: TestTree -> TestTree
    modifier =
        case testName of
            _ -> id
    in modifier $
       testHandleVsFileInDir dir testName $ \hout ->
           runTester defaultTester {tstOutput = hout} $ do
               action <- testerInterpretScriptFile inpath []
               testerLiftView action

getTestOutput :: IO TestTree
getTestOutput = do
    inpaths <- findByExtension [".in"] $ "test" </> "output"
    case inpaths of
        [] -> fail "wrong directory"
        _ -> return ()
    return $ testTree "output" $ fmap testFile inpaths
