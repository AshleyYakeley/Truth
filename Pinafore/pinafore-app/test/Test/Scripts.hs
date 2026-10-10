module Test.Scripts
    ( getTestScripts
    )
where

import Pinafore.Test
import Shapes
import Shapes.Test
import System.Directory

import Pinafore.Packages

-- Just check, don't run
testCheckScript :: FilePath -> String -> TestTree
testCheckScript fpath name =
    testTree name $ do
        runTester defaultTester{tstPackages = appPackages}
            $ testerLoadPackages appPackages
            $ do
                _ <- testerInterpretScriptFile fpath []
                return ()

testCheckScripts :: String -> FilePath -> IO TestTree
testCheckScripts name dir = do
    tnames <- listDirectory dir
    return $ testTree name $ fmap (\tname -> testCheckScript (dir </> tname) tname) $ sort tnames

testQuine :: TestName -> FilePath -> FilePath -> TestTree
testQuine name fpath outpath =
    testHandleVsFile name fpath outpath $ \hout ->
        runTester defaultTester{tstOutput = hout} $ do
            action <- testerInterpretScriptFile fpath []
            testerLiftView action

getTestScripts :: IO TestTree
getTestScripts = do
    scriptTest <- testCheckScripts "test" "test/script"
    exampleTest <- testCheckScripts "example" "examples"
    return $ testTree "scripts" [scriptTest, exampleTest, testQuine "quine" "examples/quine" "test/out/quine.out"]
