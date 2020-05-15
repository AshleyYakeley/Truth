module Test.Scripts
    ( testScripts
    ) where

import Pinafore
import Pinafore.Test
import Shapes
import Test.Tasty
import Test.Tasty.HUnit

-- Just check, don't run
testExample :: String -> TestTree
testExample fpath =
    testCase fpath $
    withTestPinaforeContext $ \_ _ _getTableState -> do
        ptext <- readFile fpath
        _ <- throwResult $ pinaforeInterpretFile fpath $ decodeUtf8 $ toStrict ptext
        return ()

testScripts :: TestTree
testScripts =
    testGroup
        "scripts"
        [ testExample "test/script/empty.pinafore"
        , testExample "test/script/simple.pinafore"
        , testExample "test/script/testquery.pinafore"
        , testExample "test/script/test.pinafore"
        , testExample "test/script/big-ui.pinafore"
        , testExample "test/script/calendar.pinafore"
        , testExample "examples/stopwatch.pinafore"
        , testExample "examples/calendar.pinafore"
        , testExample "examples/people.pinafore"
        , testExample "examples/events.pinafore"
        ]
