module Test.Golden
    ( testGolden
    ) where

import Flags
import Pinafore.Library.GNOME
import Pinafore.Test
import Shapes hiding ((.))
import Shapes.Test
import System.FilePath

testFile :: FilePath -> TestTree
testFile inpath = let
    dir = takeDirectory inpath
    testName = takeBaseName inpath
    in testHandleVsFileInDir dir testName $ \hout ->
           runTester defaultTester {tstOutput = hout} $
           testerLoadLibrary gnomeLibrary $ do
               testerLiftView $ do
                   action <- qInterpretFile inpath
                   action

items :: [String]
items =
    ["gio", "output"] <>
    mif flag_TestX11 ["window", "close-update", "listTable-selection", "listTable-insert", "locked-update"]

testItem :: String -> TestTree
testItem item = let
    file = "test" </> "golden" </> (item <> ".in")
    in testFile file

testGolden :: TestTree
testGolden = testTree "golden" $ fmap testItem items
