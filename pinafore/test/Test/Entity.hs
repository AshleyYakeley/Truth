module Test.Entity
    ( testEntity
    ) where

import Control.Exception
import Pinafore
import Pinafore.Test
import Shapes
import Test.Tasty
import Test.Tasty.HUnit

defs :: [String]
defs =
    [ "runreforfail r = runref (r ?? {fail \"unknown ref\"})"
    , "testeq expected found = runreforfail {if is %expected %found then pass else fail \"not equal\"}"
    , "testneq expected found = runreforfail {if not $ is %expected %found then pass else fail \"equal\"}"
    , "testisknown t = runref {if %(known t) then pass else fail \"known\"}"
    , "testisunknown t = runref {if %(known t) then fail \"known\" else pass}"
    , "testeqval e f = testeq {e} {f}"
    , "opentype E"
    , "eea = property @E @E !\"eea\""
    , "eeb = property @E @E !\"eeb\""
    , "eec = property @E @E !\"eec\""
    , "eed = property @E @E !\"eed\""
    , "eta = property @E @Text !\"eta\""
    , "ena = property @E @Number !\"ena\""
    , "enb = property @E @Number !\"enb\""
    , "enc = property @E @Number !\"enc\""
    , "tea = property @Text @E !\"tea\""
    , "nea = property @Number @E !\"nea\""
    , "e1 = entity @E !d4d3096a-b1f7-4cb1-8dfa-c907e57baed1"
    , "e2 = entity @E !6a76c6b3-c9d6-4e76-af3a-863b7c46b34c"
    , "e3 = entity @E !5048ecd6-bebb-4500-a508-f188b4cc7443"
    , "e4 = entity @E !6ffe864b-d2c3-4857-8057-ef472475eb2b"
    ]

prefix :: Text
prefix = pack $ "let\n" ++ intercalate ";\n" defs ++ "\nin\n"

pointTest :: Text -> TestTree
pointTest text =
    testCase (unpack text) $ do
        (pc, _getTableState) <- makeTestPinaforeContext
        action <- pinaforeInterpretFile pc "<test>" $ prefix <> text
        action

assertThrows :: IO a -> IO ()
assertThrows ma = do
    t <- catch (ma >> return True) $ \(_ :: SomeException) -> return False
    if t
        then assertFailure "no exception"
        else return ()

badParseText :: Text -> TestTree
badParseText text =
    testCase (unpack text) $ do
        (pc, _getTableState) <- makeTestPinaforeContext
        assertThrows $ pinaforeInterpretFile pc "<test>" $ prefix <> text

exceptionTest :: Text -> TestTree
exceptionTest text =
    testCase (unpack text) $ do
        (pc, _getTableState) <- makeTestPinaforeContext
        action <- pinaforeInterpretFile pc "<test>" $ prefix <> text
        assertThrows action

testEntity :: TestTree
testEntity =
    testGroup
        "entity"
        [ testGroup
              "current" -- stack test pinafore --test-arguments "--pattern entity.current"
              []
        , testGroup
              "pass"
              [ pointTest "pass"
              , pointTest "pass >> pass"
              , pointTest "if true then pass else fail \"failed\""
              , pointTest "pass >> if true then pass else fail \"failed\""
              ]
        , testGroup
              "bad parse"
              [ badParseText ""
              , badParseText "x"
              , badParseText "("
              , badParseText ")"
              , badParseText "pass x"
              , badParseText "pass pass"
              , badParseText "pass in"
              , badParseText "pass ("
              , badParseText "pass )"
              , badParseText "pass let"
              ]
        , testGroup
              "fail"
              [ exceptionTest "fail \"text\""
              , exceptionTest "let in fail \"text\""
              , exceptionTest "let t = 1 in fail \"text\""
              , exceptionTest "let opentype E in fail \"text\""
              ]
        , testGroup
              "equality"
              [ pointTest "testeqval 1 1"
              , pointTest "testeqval 1 \"1\""
              , pointTest "testeqval false $ 0 == 1"
              , pointTest "testeqval true $ 1 == 1"
              , pointTest "testeqval false $ 1 == ~1"
              ]
        , testGroup
              "unknown & known"
              [ pointTest "testisunknown {% (eta !$ {e1}) == % (eta !$ {e1})}"
              , pointTest "runreforfail {if %(known unknown) then fail \"failed\" else pass}"
              , pointTest "runreforfail {if %(known $ eta !$ {e1}) then fail \"failed\" else pass}"
              , pointTest "pass >> runreforfail {if %(known $ eta !$ {e1}) then fail \"failed\" else pass}"
              , pointTest "runreforfail {pass >> if %(known $ eta !$ {e1}) then fail \"failed\" else pass}"
              , pointTest "runreforfail {if %(known $ eta !$ {e1}) then fail \"failed\" else pass} >> pass"
              , pointTest "testisunknown unknown"
              , pointTest "testisunknown (eta !$ {e1})"
              , pointTest "testisunknown $ unknown ?? unknown"
              , pointTest "testeq {0} $ unknown ?? {0}"
              , pointTest "testeq {1} $ {1} ?? {0}"
              , pointTest "testeq {1} $ {1} ?? unknown"
              ]
        , testGroup
              ":="
              [ pointTest "eta !$ {e1} := \"hello\""
              , pointTest "eea !$ {e1} := e2"
              , pointTest "eea !$ {e1} := e2 >> testeq {e2} (eea !$ {e1})"
              , pointTest "eta !$ {e1} := \"hello\" >> testeq {\"hello\"} (eta !$ {e1})"
              , pointTest "tea !$ {\"hello\"} := e1 >> testeq {e1} (tea !$ {\"hello\"})"
              , pointTest "tea !$ {\"hello\"} := e1 >> runref {outputln (totext $ %(count (tea !@ {e1})))}"
              , pointTest "tea !$ {\"hello\"} := e1 >> testeq {1} (count (tea !@ {e1}))"
              ]
        , testGroup
              "+="
              [ pointTest "eta !@ {\"hello\"} += e1"
              , pointTest "eta !@ {\"hello\"} += e1 >> pass"
              , pointTest "eta !@ {\"hello\"} += e1 >> testeq {\"hello\"} (eta !$ {e1})"
              ]
        , testGroup
              "-="
              [pointTest "eta !@ {\"hello\"} += e1 >> eta !@ {\"hello\"} -= e1 >> testisunknown (eta !$ {e1})"]
        , testGroup
              "removeall"
              [pointTest "eta !@ {\"hello\"} += e1 >> removeall (eta !@ {\"hello\"}) >> testisunknown (eta !$ {e1})"]
        , testGroup
              "matching literals"
              [pointTest "eta !$ {e1} := \"hello\" >> eta !$ {e2} := \"hello\" >> testeq (eta !$ {e1}) (eta !$ {e2})"]
        , testGroup
              "identity morphism"
              [ pointTest "(identity !$ eea !$ {e1}) := e2 >> testeq {e2} (eea !$ {e1})"
              , pointTest "(eea !$ identity !$ {e1}) := e2 >> testeq {e2} (eea !$ {e1})"
              , pointTest "((identity !. eea) !$ {e1}) := e2 >> testeq {e2} (eea !$ {e1})"
              , pointTest "((eea !. identity) !$ {e1}) := e2 >> testeq {e2} (eea !$ {e1})"
              , pointTest "eea !$ {e1} := e2 >> testeq {e2} (identity !$ eea !$ {e1})"
              , pointTest "eea !$ {e1} := e2 >> testeq {e2} (eea !$ identity !$ {e1})"
              , pointTest "eea !$ {e1} := e2 >> testeq {e2} ((identity !. eea) !$ {e1})"
              , pointTest "eea !$ {e1} := e2 >> testeq {e2} ((eea !. identity) !$ {e1})"
              , pointTest "(identity !$ eea !$ {e1}) := e2 >> testeq {e2} (identity !$ eea !$ {e1})"
              ]
        , testGroup
              "identity inverse morphism"
              [ pointTest "(identity !@@ eta !@ {\"hello\"}) += e1 >> testisunknown (eta !$ {e1})"
              , pointTest "(eea !@@ identity !@ {e2}) += e1 >> testneq {e2} (eea !$ {e1})"
              , pointTest "((identity !. eta) !@ {\"hello\"}) += e1 >> testeq {\"hello\"} (eta !$ {e1})"
              , pointTest "((eta !. identity) !@ {\"hello\"}) += e1 >> testeq {\"hello\"} (eta !$ {e1})"
              , pointTest "eta !@ {\"hello\"} += e1 >> eta !@ {\"hello\"} -= e1 >> testisunknown (eta !$ {e1})"
              , pointTest
                    "eta !@ {\"hello\"} += e1 >> (identity !@@ eta !@ {\"hello\"}) -= e1 >> testeq {\"hello\"} (eta !$ {e1})"
              , pointTest "eea !@ {e2} += e1 >> testeq {e2} (eea !$ {e1})"
              , pointTest "eea !@ {e2} += e1 >> (eea !@@ identity !@ {e2}) -= e1 >> testneq {e2} (eea !$ {e1})"
              , pointTest
                    "eta !@ {\"hello\"} += e1 >> ((identity !. eta) !@ {\"hello\"}) -= e1 >> testisunknown (eta !$ {e1})"
              , pointTest
                    "eta !@ {\"hello\"} += e1 >> ((eta !. identity) !@ {\"hello\"}) -= e1 >> testisunknown (eta !$ {e1})"
              ]
        , testGroup
              "composed morphisms"
              [ pointTest "(eea !$ eeb !$ {e1}) := e2 >> testeq {e2} (eea !$ eeb !$ {e1})"
              , pointTest "(eta !$ eeb !$ {e1}) := \"hello\" >> testeq {\"hello\"} (eta !$ eeb !$ {e1})"
              , pointTest "(eea !. eeb !$ {e1}) := e2 >> testeq {e2} (eea !$ eeb !$ {e1})"
              , pointTest "(eta !. eeb !$ {e1}) := \"hello\" >> testeq {\"hello\"} (eta !$ eeb !$ {e1})"
              , pointTest "(eea !$ eeb !$ {e1}) := e2 >> testeq {e2} (eea !. eeb !$ {e1})"
              , pointTest "(eta !$ eeb !$ {e1}) := \"hello\" >> testeq {\"hello\"} (eta !. eeb !$ {e1})"
              , pointTest "(eeb !. eea) !$ {e2} := e1 >> testeq {e1} (eeb !$ eea !$ {e2})"
              ]
        , testGroup
              "composed inverse morphisms"
              [ pointTest "(eeb !@@ eta !@ {\"hello\"}) += e1 >> testeq {\"hello\"} (eta !$ eeb !$ {e1})"
              , pointTest "((eta !. eeb) !@ {\"hello\"}) += e1 >> testeq {\"hello\"} (eta !$ eeb !$ {e1})"
              , pointTest "((eta !. eeb) !@ {\"hello\"}) += e1 >> testisunknown (eta !$ {e1})"
              , pointTest "eeb !$ {e1} := e2 >> ((eta !. eeb) !@ {\"hello\"}) += e1 >> testeq {e2} (eeb !$ {e1})"
              , pointTest "eeb !$ {e1} := e2 >> ((eta !. eeb) !@ {\"hello\"}) += e1 >> testeq {\"hello\"} (eta !$ {e2})"
              , pointTest "eeb !$ {e1} := e2 >> (eeb !@@ eta !@  {\"hello\"}) += e1 >> testneq {e2} (eeb !$ {e1})"
              , pointTest "eeb !$ {e1} := e2 >> (eeb !@@ eta !@  {\"hello\"}) += e1 >> testisunknown (eta !$ {e2})"
              , pointTest
                    "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> ((eta !. eeb) !@ {\"hello\"}) -= e1 >> testeq {e2} (eeb !$ {e1})"
              , pointTest
                    "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> ((eta !. eeb) !@ {\"hello\"}) -= e1 >> testisunknown (eta !$ {e2})"
              , pointTest
                    "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> (eeb !@@ eta !@ {\"hello\"}) -= e1 >> testneq {e2} (eeb !$ {e1})"
              , pointTest
                    "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> (eeb !@@ eta !@ {\"hello\"}) -= e1 >> testeq {\"hello\"} (eta !$ {e2})"
              , pointTest
                    "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> removeall ((eta !. eeb) !@ {\"hello\"}) >> testeq {e2} (eeb !$ {e1})"
              , pointTest
                    "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> removeall ((eta !. eeb) !@ {\"hello\"}) >> testisunknown (eta !$ {e2})"
              , pointTest
                    "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> removeall (eeb !@@ eta !@ {\"hello\"}) >> testneq {e2} (eeb !$ {e1})"
              , pointTest
                    "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> removeall (eeb !@@ eta !@ {\"hello\"}) >> testeq {\"hello\"} (eta !$ {e2})"
              ]
        , testGroup
              "single"
              [ pointTest "testisunknown (single $ enb !$$ ena !@ {0})"
              , pointTest "enb !$ {e1} := 1 >> ena !$ {e1} := 0 >> testeq {1} (single $ enb !$$ ena !@ {0})"
              , pointTest
                    "enb !$ {e1} := 1 >> ena !$ {e1} := 0 >> enc !$ {e1} := 0 >> testeq {1} (single $ enb !$$ ena !@ {0})"
              , pointTest
                    "enb !$ {e1} := 1 >> ena !$ {e1} := 0 >> ena !$ {e1} := 0 >> testeq {1} (single $ enb !$$ ena !@ {0})"
              , pointTest
                    "enb !$ {e1} := 1 >> enb !$ {e2} := 2 >> ena !$ {e1} := 0 >> ena !$ {e2} := 0 >> testisunknown (single $ enb !$$ ena !@ {0})"
              , pointTest
                    "enb !$ {e1} := 1 >> enb !$ {e2} := 1 >> ena !$ {e1} := 0 >> ena !$ {e2} := 0 >> testeq {1} (single $ enb !$$ ena !@ {0})"
              ]
        , testGroup
              "multiple set member"
              [ pointTest "testeq {0} (count (tea !@ {e1}))"
              , pointTest "tea !@ {e1} += \"hello\" >> testeq {e1} (tea !$ {\"hello\"})"
              , pointTest "tea !@ {e1} += \"hello\" >> testeq {1} (count (tea !@ {e1}))"
              , pointTest "tea !@ {e1} += \"hello\" >> tea !@ {e1} += \"hello\" >> testeq {1} (count (tea !@ {e1}))"
              , pointTest "tea !@ {e1} += \"h\" >> tea !@ {e1} += \"hello\" >> testeq {2} (count (tea !@ {e1}))"
              , pointTest "eea !$ {e2} := e1 >> testeq {1} (count (eea !@ {e1}))"
              , pointTest $
                "let counter = ena !$ {e1};someset = nea !@ {e1} in " <>
                "counter := 0 >> someset += 1 >> someset += 1 >> (get (members (orders []) someset) $ \\pp -> for pp $ \\p -> runref {counter := %counter + 1}) >> testeq {1} counter"
              ]
        , testGroup
              "Types"
              [ pointTest "let opentype T1; p = property @T1 @T1 !\"p\" in pass"
              , pointTest "let opentype T1 in let p = property @T1 @T1 !\"p\" in pass"
              , pointTest "let opentype T1; opentype T2; p = property @T1 @T2 !\"p\" in pass"
              , pointTest "let opentype T1; opentype T2 in let p = property @T1 @T2 !\"p\" in pass"
              , pointTest "let opentype T1 in let opentype T2; p = property @T1 @T2 !\"p\" in pass"
              , pointTest "let opentype T1 in let opentype T2 in let p = property @T1 @T2 !\"p\" in pass"
              ]
        , testGroup
              "Pair/Either"
              [ pointTest
                    "let enta = property @E @(Number, Text) !\"enta\" in enta !$ {e1} := (74,\"hmm\") >> (testneq {(71,\"hmm\")} $ enta !$ {e1})"
              , pointTest
                    "let enta = property @E @(Number, Text) !\"enta\" in enta !$ {e1} := (74,\"hmm\") >> (testeq {(74,\"hmm\")} $ enta !$ {e1})"
              , pointTest
                    "let enta = property @E @(Either Number Text) !\"enta\" in enta !$ {e1} := left 74 >> (testneq {left 73} $ enta !$ {e1})"
              , pointTest
                    "let enta = property @E @(Either Number Text) !\"enta\" in enta !$ {e1} := left 74 >> (testeq {left 74} $ enta !$ {e1})"
              , pointTest
                    "let enta = property @E @(Either Number Text) !\"enta\" in enta !$ {e1} := right \"abc\" >> (testneq {right \"adbc\"} $ enta !$ {e1})"
              , pointTest
                    "let enta = property @E @(Either Number Text) !\"enta\" in enta !$ {e1} := right \"abc\" >> (testeq {right \"abc\"} $ enta !$ {e1})"
              ]
        ]
