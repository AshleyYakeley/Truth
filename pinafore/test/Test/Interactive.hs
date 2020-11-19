module Test.Interactive
    ( getTestInteractive
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
    in testHandleVsFile dir testName $ \outh ->
           withBinaryFile inpath ReadMode $ \inh ->
               withTestPinaforeContext nullFetchModuleText outh $ \tc _ _ -> do
                   tcRunView tc emptyResourceContext $ pinaforeInteractHandles inh outh True
                   hPutStrLn outh "<END>"

getTestInteractive :: IO TestTree
getTestInteractive = do
    inpaths <- findByExtension [".in"] $ "test" </> "interactive"
    return $ testTree "interactive" $ fmap testFile inpaths
