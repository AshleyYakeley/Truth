module Test.Golden
    ( testGolden
    )
where

import Pinafore.Library.Media
import Pinafore.Test
import Shapes hiding ((.))
import Shapes.Test
import System.FilePath

import Flags
import Pinafore.Library.GNOME

testFile :: FilePath -> TestTree
testFile inpath = let
    dir = takeDirectory inpath
    testName = takeBaseName inpath
    in testHandleVsFileInDir dir testName $ \hout ->
        runTester defaultTester{tstOutput = hout}
            $ testerLoadLibrary (mediaLibrary <> gnomeLibrary)
            $ do
                action <- testerInterpretScriptFile inpath []
                testerLiftView action

items :: [String]
items =
    ["gio", "output"]
        <> mif flag_TestX11 ["window", "close-update", "listTable-selection", "listTable-insert", "locked-update"]

testItem :: String -> TestTree
testItem item = let
    file = "test" </> "golden" </> (item <> ".in")
    in testFile file

testGolden :: TestTree
testGolden = testTree "golden" $ fmap testItem items
