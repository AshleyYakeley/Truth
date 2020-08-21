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
        [ testExample "test/script/empty"
        , testExample "test/script/simple"
        , testExample "test/script/testquery"
        , testExample "test/script/test"
        , testExample "test/script/big-ui"
        , testExample "test/script/calendar"
        , testExample "examples/stopwatch"
        , testExample "examples/calendar"
        , testExample "examples/people"
        , testExample "examples/events"
        ]
