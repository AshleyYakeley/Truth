module Test.Output
    ( getTestOutput
    ) where

import Changes.Core
import Pinafore
import Pinafore.Test
import Shapes hiding ((<.>))
import Shapes.Test
import System.FilePath

testFile :: FilePath -> TestTree
testFile inpath = let
    dir = takeDirectory inpath
    testName = takeBaseName inpath
    tmod =
        case testName of
            "recursive-1" -> failTestBecause "ISSUE #113"
            "recursive-2" -> failTestBecause "ISSUE #113"
            _ -> id
    in tmod $
       testHandleVsFile dir testName $ \hout ->
           withTestPinaforeContext mempty hout $ \cc _ _ -> do
               action <- pinaforeInterpretFile inpath
               ccRunView cc emptyResourceContext action
               return ()

getTestOutput :: IO TestTree
getTestOutput = do
    inpaths <- findByExtension [".in"] $ "test" </> "output"
    return $ testTree "output" $ fmap testFile inpaths
