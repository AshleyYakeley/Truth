module Test.Scripts
    ( testScripts
    ) where

import Pinafore
import Pinafore.Language.Library.GTK
import Pinafore.Test
import Shapes
import Shapes.Test

-- Just check, don't run
testExample :: String -> TestTree
testExample fpath =
    testTree fpath $
    withTestPinaforeContext (libraryFetchModule gtkLibrary <> directoryFetchModule "examples") stdout $ \_ _ _getTableState -> do
        _ <- pinaforeInterpretFile fpath
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
