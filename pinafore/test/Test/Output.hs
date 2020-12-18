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
    in testHandleVsFile dir testName $ \hout ->
           withTestPinaforeContext mempty hout $ \tc _ _ -> do
               action <- pinaforeInterpretFile inpath
               tcRunView tc emptyResourceContext action
               return ()

getTestOutput :: IO TestTree
getTestOutput = do
    inpaths <- findByExtension [".in"] $ "test" </> "output"
    return $ testTree "output" $ fmap testFile inpaths
