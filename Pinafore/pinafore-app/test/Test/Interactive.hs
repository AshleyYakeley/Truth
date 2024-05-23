module Test.Interactive
    ( getTestInteractive
    ) where

import Pinafore.Libs
import Pinafore.Main
import Pinafore.Test
import Shapes hiding ((.))
import Shapes.Test
import System.FilePath

testFile :: FilePath -> TestTree
testFile inpath = let
    dir = takeDirectory inpath
    testName = takeBaseName inpath
    in testHandleVsFileInDir dir testName $ \outh ->
           withBinaryFile inpath ReadMode $ \inh ->
               runTester defaultTester {tstFetchModule = libraryFetchModule () extraLibrary, tstOutput = outh} $ do
                   testerLiftView $ qInteractHandles inh outh True
                   liftIO $ hPutStrLn outh "<END>"

getTestInteractive :: IO TestTree
getTestInteractive = do
    inpaths <- findByExtension [".in"] $ "test" </> "interactive"
    case inpaths of
        [] -> fail "wrong directory"
        _ -> return ()
    return $ testTree "interactive" $ fmap testFile inpaths
