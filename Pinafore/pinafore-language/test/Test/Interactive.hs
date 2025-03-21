module Test.Interactive
    ( getTestInteractive
    )
where

import Shapes hiding ((.))
import Shapes.Test
import System.FilePath

import Pinafore.Test.Internal

testFile :: FilePath -> TestTree
testFile inpath = let
    dir = takeDirectory inpath
    testName = takeBaseName inpath
    in testHandleVsFileInDir dir testName $ \outh ->
        withBinaryFile inpath ReadMode $ \inh ->
            runTester defaultTester{tstOutput = outh} $ do
                testerLiftView $ qInteractHandles inh outh True
                liftIO $ hPutStrLn outh "<END>"

getTestInteractive :: IO TestTree
getTestInteractive = do
    inpaths <- findByExtension [".in"] $ "test" </> "interactive"
    case inpaths of
        [] -> fail "wrong directory"
        _ -> return ()
    return $ testTree "interactive" $ fmap testFile inpaths
