module Test.Golden
    ( testGolden
    )
where

import Pinafore.Test
import Shapes hiding ((.))
import Shapes.Test
import System.FilePath

import Pinafore.Library.Media

testFile :: FilePath -> TestTree
testFile inpath = let
    dir = takeDirectory inpath
    testName = takeBaseName inpath
    in testHandleVsFileInDir dir testName $ \hout ->
        runTester defaultTester{tstOutput = hout}
            $ testerLoadLibrary mediaLibrary
            $ do
                action <- testerInterpretScriptFile inpath []
                testerLiftView action

items :: [String]
items =
    ["uri"]

testItem :: String -> TestTree
testItem item = let
    file = "test" </> "golden" </> (item <> ".in")
    in testFile file

testGolden :: TestTree
testGolden = testTree "golden" $ fmap testItem items
