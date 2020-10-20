module Test.Scripts
    ( testScripts
    ) where

import Pinafore
import Pinafore.Test
import Shapes
import Shapes.Test

-- Just check, don't run
testExample :: String -> TestTree
testExample fpath =
    testTree fpath $
    withTestPinaforeContext $ \_ _ _getTableState -> do
        ptext <- readFile fpath
        _ <- throwResult $ pinaforeInterpretFile fpath $ decodeUtf8 $ toStrict ptext
        return ()

testScripts :: TestTree
testScripts =
    testTree
        "scripts"
        [ testExample "test/script/empty"
        , testExample "test/script/simple"
        , testExample "test/script/testquery"
        , testExample "test/script/test"
        , testExample "test/script/big-ui"
        , testExample "test/script/calendar"
        , testExample "examples/stopwatch"
        , testExample "examples/calendar"
        , testExample "examples/contacts"
        , testExample "examples/events"
        ]
