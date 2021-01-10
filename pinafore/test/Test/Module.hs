module Test.Module
    ( testModule
    ) where

import Pinafore
import Pinafore.Test
import Shapes
import Shapes.Test
import Test.RunScript

moduleRunTest :: ScriptExpectation -> [(Text, Text)] -> Text -> TestTree
moduleRunTest expect modules text = let
    fetchModule :: FetchModule
    fetchModule = textFetchModule $ \mname -> return $ lookup (pack $ show mname) modules
    in testPinaforeScript text fetchModule text expect []

testModule :: TestTree
testModule =
    testTree
        "module"
        [ moduleRunTest ScriptExpectSuccess [] "return ()"
        , moduleRunTest ScriptExpectSuccess [] "let a=4 in if a == 4 then return () else fail \"wrong\""
        , moduleRunTest
              ScriptExpectSuccess
              [("M", "let a=4 in export a")]
              "let import M in if a == 4 then return () else fail \"wrong\""
        , moduleRunTest
              ScriptExpectSuccess
              [("M", "let a=4 in export a")]
              "let import M; b = a in if b == 4 then return () else fail \"wrong\""
        , moduleRunTest
              ScriptExpectSuccess
              [("M", "let datatype T = T1 | T2 in export T T1 T2")]
              "let import M in case T1 of T1 -> return (); T2 -> fail \"wrong\" end"
        , moduleRunTest
              ScriptExpectSuccess
              [("M", "let datatype T = T1 | T2 in export T T1 T2")]
              "let import M in let t: T; t = T1 in case t of T1 -> return (); T2 -> fail \"wrong\" end"
        , moduleRunTest
              ScriptExpectSuccess
              [("M", "let datatype T = T1 | T2 in export T T1 T2")]
              "let import M; t: T; t = T1 in case t of T1 -> return (); T2 -> fail \"wrong\" end"
        , moduleRunTest
              ScriptExpectSuccess
              [("M", "let datatype T = T1 | T2 in export T T1 T2")]
              "let import M; f: T -> T; f x = x in return ()"
        , moduleRunTest
              ScriptExpectSuccess
              [("M", "let datatype T = T1 | T2 in export T T1 T2")]
              "let import M in let f: T -> T; f x = x in return ()"
        ]
