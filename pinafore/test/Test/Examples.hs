module Test.Examples
    ( testExamples
    ) where

import Pinafore
import Pinafore.Test
import Shapes
import Test.Tasty
import Test.Tasty.HUnit

-- Just check, don't run
testExample :: String -> TestTree
testExample fpath =
    testCase fpath $ do
        (context, _getTableState) <- makeTestPinaforeContext
        ptext <- readFile $ "examples/" <> fpath
        _ <- pinaforeInterpretFile context fpath $ decodeUtf8 $ toStrict ptext
        return ()

testExamples :: TestTree
testExamples =
    testGroup
        "examples"
        [ testExample "empty.pinafore"
        , testExample "simple.pinafore"
        , testExample "testquery.pinafore"
        , testExample "test.pinafore"
        ]
