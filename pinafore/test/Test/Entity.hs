module Test.Entity
    ( testEntity
    , testUpdates
    ) where

import Changes.Core
import Pinafore
import Pinafore.Test
import Shapes
import Shapes.Test
import Test.Context
import Test.RunScript

testUpdate :: Text -> ContextTestTree
testUpdate text =
    testPinaforeScript text mempty text $
    ScriptExpectSuccessResult $ \cc (sendUpdate, ref) ->
        runEditor emptyResourceContext (unWModel $ immutableRefToRejectingRef ref) $
        checkUpdateEditor (Known (1 :: Integer)) $
        tcUnliftLifeCycle cc $ tcRunView cc emptyResourceContext $ unliftPinaforeActionOrFail sendUpdate

testUpdates :: TestTree
testUpdates = runContext $ tgroup "update" [testUpdate "do ref <- newMemWhole; return (ref := 1, ref) end"]

testEntity :: TestTree
testEntity =
    runContext $
    context
        [ "pass = return ()"
        , "runWholeRef r = do a <- get r; a end"
        , "runreforfail r = runWholeRef (r ?? {fail \"unknown ref\"})"
        , "testeq expected found = runreforfail {if %expected == %found then pass else fail \"not equal\"}"
        , "testneq expected found = runreforfail {if %expected /= %found then pass else fail \"equal\"}"
        , "testisknown t = runWholeRef {if %(known t) then pass else fail \"known\"}"
        , "testisunknown t = runWholeRef {if %(known t) then fail \"known\" else pass}"
        , "testeqval e f = testeq {e} {f}"
        , "expectStop p = onStop (p >> fail \"no stop\") pass"
        ] $
    tgroup
        "entity"
        [ context [] $
          tgroup
              "current" -- stack test pinafore --test-arguments "--pattern entity.current"
              []
        , tgroup
              "pass"
              [ testExpectSuccess "pass"
              , testExpectSuccess "pass >> pass"
              , testExpectSuccess "if True then pass else fail \"failed\""
              , testExpectSuccess "pass >> if True then pass else fail \"failed\""
              ]
        , tgroup
              "bad parse"
              [ testExpectReject ""
              , testExpectReject "x"
              , testExpectReject "("
              , testExpectReject ")"
              , testExpectReject "pass x"
              , testExpectReject "pass pass"
              , testExpectReject "pass in"
              , testExpectReject "pass ("
              , testExpectReject "pass )"
              , testExpectReject "pass let"
              ]
        , tgroup
              "fail"
              [ testExpectStop "stop"
              , testExpectThrow "fail \"text\""
              , testExpectThrow "let in fail \"text\""
              , testExpectThrow "let t = 1 in fail \"text\""
              , testExpectThrow "let opentype T in fail \"text\""
              ]
        , tgroup
              "do"
              [ testExpectSuccess "do return () end"
              , testExpectSuccess "do return (); end"
              , testExpectSuccess "do testeqval 3 3 end"
              , testExpectSuccess "do a <- return 3; testeqval 3 a end"
              , testExpectSuccess "do a <- return 3; b <- return $ a + a; testeqval 6 b end"
              ]
        , context ["flagRef = do r <- newMemWhole; r := False; return r; end"] $
          tgroup
              "stop"
              [ testExpectSuccess "return ()"
              , testExpectThrow "fail \"failure\""
              , testExpectSuccess "expectStop stop"
              , testExpectSuccess "expectStop $ do stop; fail \"unstopped\"; end"
              , testExpectSuccess "do a <- onStop (return 1) (return 2); testeqval 1 a; end"
              , testExpectSuccess "do a <- onStop (return 1) stop; testeqval 1 a; end"
              , testExpectThrow "do a <- onStop (return 1) stop; fail \"unstopped\"; end"
              , testExpectSuccess "do a <- onStop stop (return 2); testeqval 2 a; end"
              , testExpectThrow "do a <- onStop stop (return 2); fail \"unstopped\"; end"
              , testExpectSuccess
                    "do r1 <- flagRef; r2 <- flagRef; onStop (r1 := True) (r2 := True); testeq {True} r1; testeq {False} r2; end"
              , testExpectSuccess
                    "do r1 <- flagRef; r2 <- flagRef; onStop (do r1 := True; stop; end) (r2 := True); testeq {True} r1; testeq {True} r2; end"
              , testExpectSuccess
                    "do r1 <- flagRef; r2 <- flagRef; onStop (do stop; r1 := True; end) (r2 := True); testeq {False} r1; testeq {True} r2; end"
              ]
        , tgroup
              "equality"
              [ testExpectSuccess "testeqval 1 1"
              , testExpectSuccess "testeqval 1 \"1\""
              , testExpectSuccess "testeqval False $ 0 == 1"
              , testExpectSuccess "testeqval True $ 1 == 1"
              , testExpectSuccess "testeqval False $ 1 == ~1"
              ]
        , tgroup
              "reference notation"
              [ testExpectSuccess "runreforfail {pass}"
              , testExpectSuccess "let p = pass in runreforfail {p}"
              , testExpectSuccess "runreforfail {let p = pass in p}"
              , testExpectSuccess "runreforfail {%{pass}}"
              , testExpectSuccess "let rp = {pass} in runreforfail {%rp}"
              , testExpectSuccess "runreforfail {let rp = {pass} in %rp}"
              , testExpectSuccess "let rp = {pass} in runreforfail {let p= %rp in p}"
              ]
        , tgroup
              "reference stop"
              [ testExpectSuccess "expectStop $ stop"
              , testExpectSuccess "expectStop $ get unknown"
              , testExpectSuccess "expectStop $ {1} := 1"
              , testExpectSuccess "expectStop $ delete {1}"
              ]
        , tgroup
              "memory references"
              [ testExpectSuccess "expectStop $ do r <- newMemWhole; get r; end"
              , testExpectSuccess "do r <- newMemWhole; r := 45; a <- get r; testeqval 45 a; end"
              , testExpectSuccess "do r <- newMemWhole; r := 3; r := 4; a <- get r; testeqval 4 a; end"
              , testExpectSuccess "do s <- newMemFiniteSet; n <- get $ count s; testeqval 0 n; end"
              , testExpectSuccess "do s <- newMemFiniteSet; s += 57; n <- get $ count s; testeqval 1 n; end"
              , testExpectSuccess "do s <- newMemFiniteSet; s -= 57; n <- get $ count s; testeqval 0 n; end"
              , testExpectSuccess "do s <- newMemFiniteSet; s += 57; s -= 57; n <- get $ count s; testeqval 0 n; end"
              , testExpectSuccess
                    "do s <- newMemFiniteSet; s += 57; m54 <- get $ member s {54}; m57 <- get $ member s {57}; testeqval False m54; testeqval True m57; end"
              , testExpectSuccess
                    "do s <- newMemFiniteSet; s -= 57; m57 <- get $ member s {57}; testeqval False m57; end"
              , testExpectSuccess
                    "do s <- newMemFiniteSet; s += 57; s -= 57; m57 <- get $ member s {57}; testeqval False m57; end"
              , testExpectSuccess
                    "do s <- newMemFiniteSet; member s {57} := True; m54 <- get $ member s {54}; m57 <- get $ member s {57}; testeqval False m54; testeqval True m57; end"
              , testExpectSuccess
                    "do s <- newMemFiniteSet; member s {57} := False; m57 <- get $ member s {57}; testeqval False m57; end"
              , testExpectSuccess
                    "do s <- newMemFiniteSet; member s {57} := True; member s {57} := False; m57 <- get $ member s {57}; testeqval False m57; end"
              , testExpectSuccess "expectStop $ do r <- newMemWhole; immutWhole r := 5; end"
              ]
        , context
              [ "convr : Rational -> Rational;convr = id"
              , "convn : Number -> Number;convn = id"
              , "convl : Literal -> Literal;convl = id"
              , "testconvr : Rational -> Action ();testconvr r = testeq {convl r} {convl $ convn r}"
              ] $
          tgroup
              "literal conversion"
              [ tgroup
                    "Rational to Number"
                    [ testExpectSuccess "testconvr 1"
                    , testExpectSuccess "testconvr 2.5"
                    , testExpectSuccess "testeq {convl 31.5} {convl $ convn 31.5}"
                    , testExpectSuccess "testeq {\"63/2\"} {toText 31.5}"
                    , testExpectSuccess "testeq {\"63/2\"} {toText $ convn 31.5}"
                    ]
              ]
        , context
              [ "opentype E"
              , "eea = property @E @E !\"eea\""
              , "eeb = property @E @E !\"eeb\""
              , "eec = property @E @E !\"eec\""
              , "eed = property @E @E !\"eed\""
              , "eta = property @E @Text !\"eta\""
              , "eia = property @E @Integer !\"eia\""
              , "eib = property @E @Integer !\"eib\""
              , "eic = property @E @Integer !\"eic\""
              , "tea = property @Text @E !\"tea\""
              , "nea = property @Integer @E !\"nea\""
              , "e1 = openEntity @E !\"e1\""
              , "e2 = openEntity @E !\"e2\""
              , "e3 = openEntity @E !\"e3\""
              , "e4 = openEntity @E !\"e4\""
              , "eba = property @E @Boolean !\"eba\""
              , "era = property @E @Rational !\"era\""
              , "ena = property @E @Number !\"ena\""
              ] $
          tgroup
              "Storage"
              [ tgroup
                    "unknown & known"
                    [ testExpectSuccess "testisunknown {% (eta !$ {e1}) == % (eta !$ {e1})}"
                    , testExpectSuccess "runreforfail {if %(known unknown) then fail \"failed\" else pass}"
                    , testExpectSuccess "runreforfail {if %(known $ eta !$ {e1}) then fail \"failed\" else pass}"
                    , testExpectSuccess
                          "pass >> runreforfail {if %(known $ eta !$ {e1}) then fail \"failed\" else pass}"
                    , testExpectSuccess
                          "runreforfail {pass >> if %(known $ eta !$ {e1}) then fail \"failed\" else pass}"
                    , testExpectSuccess
                          "runreforfail {if %(known $ eta !$ {e1}) then fail \"failed\" else pass} >> pass"
                    , testExpectSuccess "testisunknown unknown"
                    , testExpectSuccess "testisunknown (eta !$ {e1})"
                    , testExpectSuccess "testisunknown $ unknown ?? unknown"
                    , testExpectSuccess "testeq {0} $ unknown ?? {0}"
                    , testExpectSuccess "testeq {1} $ {1} ?? {0}"
                    , testExpectSuccess "testeq {1} $ {1} ?? unknown"
                    ]
              , tgroup
                    ":="
                    [ testExpectSuccess "eta !$ {e1} := \"hello\""
                    , testExpectSuccess "eea !$ {e1} := e2"
                    , testExpectSuccess "eea !$ {e1} := e2 >> testeq {e2} (eea !$ {e1})"
                    , testExpectSuccess "eta !$ {e1} := \"hello\" >> testeq {\"hello\"} (eta !$ {e1})"
                    , testExpectSuccess "tea !$ {\"hello\"} := e1 >> testeq {e1} (tea !$ {\"hello\"})"
                    , testExpectSuccess
                          "tea !$ {\"hello\"} := e1 >> runWholeRef {outputLn (toText $ %(count (tea !@ {e1})))}"
                    , testExpectSuccess "tea !$ {\"hello\"} := e1 >> testeq {1} (count (tea !@ {e1}))"
                    , testExpectSuccess "(eea !. eea) !$ {e1} := e2"
                    , testExpectSuccess
                          "do (eea !. eea) !$ {e1} := e2; testeq {e2} ((eea !. eea) !$ {e1}); testeq {e2} (eea !$ (eea !$ {e1})); end"
                    , testExpectSuccess
                          "do eea !$ (eea !$ {e1}) := e2; testeq {e2} ((eea !. eea) !$ {e1}); testeq {e2} (eea !$ (eea !$ {e1})); end"
                    , testExpectSuccess "expectStop $ do r <- newMemWhole; eia !$ r := 4; end"
                    ]
              , tgroup
                    "+="
                    [ testExpectSuccess "eta !@ {\"hello\"} += e1"
                    , testExpectSuccess "eta !@ {\"hello\"} += e1 >> pass"
                    , testExpectSuccess "eta !@ {\"hello\"} += e1 >> testeq {\"hello\"} (eta !$ {e1})"
                    ]
              , tgroup
                    "-="
                    [ testExpectSuccess
                          "eta !@ {\"hello\"} += e1 >> eta !@ {\"hello\"} -= e1 >> testisunknown (eta !$ {e1})"
                    ]
              , tgroup
                    "removeAll"
                    [ testExpectSuccess
                          "eta !@ {\"hello\"} += e1 >> removeAll (eta !@ {\"hello\"}) >> testisunknown (eta !$ {e1})"
                    ]
              , tgroup
                    "literal storage"
                    [ tgroup
                          "Boolean"
                          [ testExpectSuccess "eba !$ {e1} := True >> testeq {True} (eba !$ {e1})"
                          , testExpectSuccess "eba !$ {e1} := False >> testeq {False} (eba !$ {e1})"
                          ]
                    , tgroup
                          "Text"
                          [ testExpectSuccess "eta !$ {e1} := \"\" >> testeq {\"\"} (eta !$ {e1})"
                          , testExpectSuccess "eta !$ {e1} := \"hello\" >> testeq {\"hello\"} (eta !$ {e1})"
                          ]
                    , tgroup
                          "Integer"
                          [ testExpectSuccess "eia !$ {e1} := 0 >> testeq {0} (eia !$ {e1})"
                          , testExpectSuccess "eia !$ {e1} := 47 >> testeq {47} (eia !$ {e1})"
                          , testExpectSuccess "eia !$ {e1} := -12 >> testeq {-12} (eia !$ {e1})"
                          ]
                    , tgroup
                          "Rational"
                          [ testExpectSuccess "era !$ {e1} := 0 >> testeq {0} (era !$ {e1})"
                          , testExpectSuccess "era !$ {e1} := 47 >> testeq {47} (era !$ {e1})"
                          , testExpectSuccess "era !$ {e1} := -12 >> testeq {-12} (era !$ {e1})"
                          , testExpectSuccess "era !$ {e1} := 31.5 >> testeq {31.5} (era !$ {e1})"
                          , testExpectSuccess "era !$ {e1} := -22.8_70 >> testeq {-22.8_70} (era !$ {e1})"
                          ]
                    , tgroup
                          "Number"
                          [ testExpectSuccess "ena !$ {e1} := 0 >> testeq {0} (ena !$ {e1})"
                          , testExpectSuccess "ena !$ {e1} := 47 >> testeq {47} (ena !$ {e1})"
                          , testExpectSuccess "ena !$ {e1} := -12 >> testeq {-12} (ena !$ {e1})"
                          , testExpectSuccess "ena !$ {e1} := 31.5 >> testeq {31.5} (ena !$ {e1})"
                          , testExpectSuccess "ena !$ {e1} := -22.8_70 >> testeq {-22.8_70} (ena !$ {e1})"
                          , testExpectSuccess "ena !$ {e1} := ~36.4 >> testeq {~36.4} (ena !$ {e1})"
                          , testExpectSuccess "ena !$ {e1} := ~-22.1 >> testeq {~-22.1} (ena !$ {e1})"
                          , testExpectSuccess "ena !$ {e1} := ~-0 >> testeq {~-0} (ena !$ {e1})"
                          , testExpectSuccess "ena !$ {e1} := (0 ~/ 0) >> testeq {0 ~/ 0} (ena !$ {e1})"
                          , testExpectSuccess "ena !$ {e1} := (1 ~/ 0) >> testeq {1 ~/ 0} (ena !$ {e1})"
                          , testExpectSuccess "ena !$ {e1} := (-1 ~/ 0) >> testeq {-1 ~/ 0} (ena !$ {e1})"
                          ]
                    ]
              , tgroup
                    "matching literals"
                    [ testExpectSuccess
                          "eta !$ {e1} := \"hello\" >> eta !$ {e2} := \"hello\" >> testeq (eta !$ {e1}) (eta !$ {e2})"
                    ]
              , tgroup
                    "identity morphism"
                    [ testExpectSuccess "(identity !$ eea !$ {e1}) := e2 >> testeq {e2} (eea !$ {e1})"
                    , testExpectSuccess "(eea !$ identity !$ {e1}) := e2 >> testeq {e2} (eea !$ {e1})"
                    , testExpectSuccess "((identity !. eea) !$ {e1}) := e2 >> testeq {e2} (eea !$ {e1})"
                    , testExpectSuccess "((eea !. identity) !$ {e1}) := e2 >> testeq {e2} (eea !$ {e1})"
                    , testExpectSuccess "eea !$ {e1} := e2 >> testeq {e2} (identity !$ eea !$ {e1})"
                    , testExpectSuccess "eea !$ {e1} := e2 >> testeq {e2} (eea !$ identity !$ {e1})"
                    , testExpectSuccess "eea !$ {e1} := e2 >> testeq {e2} ((identity !. eea) !$ {e1})"
                    , testExpectSuccess "eea !$ {e1} := e2 >> testeq {e2} ((eea !. identity) !$ {e1})"
                    , testExpectSuccess "(identity !$ eea !$ {e1}) := e2 >> testeq {e2} (identity !$ eea !$ {e1})"
                    ]
              , tgroup
                    "identity inverse morphism"
                    [ testExpectSuccess "(identity !@@ eta !@ {\"hello\"}) += e1 >> testeq {\"hello\"} (eta !$ {e1})"
                    , testExpectSuccess "(eea !@@ identity !@ {e2}) += e1 >> testneq {e2} (eea !$ {e1})"
                    , testExpectSuccess "(eta !@ {\"hello\"}) += e1 >> testeq {\"hello\"} (eta !$ {e1})"
                    , testExpectSuccess "((identity !. eta) !@ {\"hello\"}) += e1 >> testeq {\"hello\"} (eta !$ {e1})"
                    , testExpectSuccess "((eta !. identity) !@ {\"hello\"}) += e1 >> testeq {\"hello\"} (eta !$ {e1})"
                    , testExpectSuccess
                          "eta !@ {\"hello\"} += e1 >> eta !@ {\"hello\"} -= e1 >> testisunknown (eta !$ {e1})"
                    , testExpectSuccess
                          "eta !@ {\"hello\"} += e1 >> (identity !@@ eta !@ {\"hello\"}) -= e1 >> testeq {\"hello\"} (eta !$ {e1})"
                    , testExpectSuccess "eea !@ {e2} += e1 >> testeq {e2} (eea !$ {e1})"
                    , testExpectSuccess
                          "eea !@ {e2} += e1 >> (eea !@@ identity !@ {e2}) -= e1 >> testneq {e2} (eea !$ {e1})"
                    , testExpectSuccess
                          "eta !@ {\"hello\"} += e1 >> ((identity !. eta) !@ {\"hello\"}) -= e1 >> testisunknown (eta !$ {e1})"
                    , testExpectSuccess
                          "eta !@ {\"hello\"} += e1 >> ((eta !. identity) !@ {\"hello\"}) -= e1 >> testisunknown (eta !$ {e1})"
                    ]
              , tgroup
                    "composed morphisms"
                    [ testExpectSuccess "(eea !$ eeb !$ {e1}) := e2 >> testeq {e2} (eea !$ eeb !$ {e1})"
                    , testExpectSuccess "(eta !$ eeb !$ {e1}) := \"hello\" >> testeq {\"hello\"} (eta !$ eeb !$ {e1})"
                    , testExpectSuccess "(eea !. eeb !$ {e1}) := e2 >> testeq {e2} (eea !$ eeb !$ {e1})"
                    , testExpectSuccess "(eta !. eeb !$ {e1}) := \"hello\" >> testeq {\"hello\"} (eta !$ eeb !$ {e1})"
                    , testExpectSuccess "(eea !$ eeb !$ {e1}) := e2 >> testeq {e2} (eea !. eeb !$ {e1})"
                    , testExpectSuccess "(eta !$ eeb !$ {e1}) := \"hello\" >> testeq {\"hello\"} (eta !. eeb !$ {e1})"
                    , testExpectSuccess "(eeb !. eea) !$ {e2} := e1 >> testeq {e1} (eeb !$ eea !$ {e2})"
                    ]
              , tgroup
                    "composed inverse morphisms"
                    [ testExpectSuccess "(eeb !@@ eta !@ {\"hello\"}) += e1 >> testeq {\"hello\"} (eta !$ eeb !$ {e1})"
                    , testExpectSuccess "((eta !. eeb) !@ {\"hello\"}) += e1 >> testeq {\"hello\"} (eta !$ eeb !$ {e1})"
                    , testExpectSuccess "((eta !. eeb) !@ {\"hello\"}) += e1 >> testisunknown (eta !$ {e1})"
                    , testExpectSuccess
                          "eeb !$ {e1} := e2 >> ((eta !. eeb) !@ {\"hello\"}) += e1 >> testeq {e2} (eeb !$ {e1})"
                    , testExpectSuccess
                          "eeb !$ {e1} := e2 >> ((eta !. eeb) !@ {\"hello\"}) += e1 >> testeq {\"hello\"} (eta !$ {e2})"
                    , testExpectSuccess
                          "eeb !$ {e1} := e2 >> (eeb !@@ eta !@  {\"hello\"}) += e1 >> testeq {e2} (eeb !$ {e1})"
                    , testExpectSuccess
                          "eeb !$ {e1} := e2 >> (eeb !@@ eta !@  {\"hello\"}) += e1 >> testeq {\"hello\"} (eta !$ {e2})"
                    , testExpectSuccess
                          "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> ((eta !. eeb) !@ {\"hello\"}) -= e1 >> testeq {e2} (eeb !$ {e1})"
                    , testExpectSuccess
                          "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> ((eta !. eeb) !@ {\"hello\"}) -= e1 >> testisunknown (eta !$ {e2})"
                    , testExpectSuccess
                          "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> (eeb !@@ eta !@ {\"hello\"}) -= e1 >> testneq {e2} (eeb !$ {e1})"
                    , testExpectSuccess
                          "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> (eeb !@@ eta !@ {\"hello\"}) -= e1 >> testeq {\"hello\"} (eta !$ {e2})"
                    , testExpectSuccess
                          "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> removeAll ((eta !. eeb) !@ {\"hello\"}) >> testeq {e2} (eeb !$ {e1})"
                    , testExpectSuccess
                          "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> removeAll ((eta !. eeb) !@ {\"hello\"}) >> testisunknown (eta !$ {e2})"
                    , testExpectSuccess
                          "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> removeAll (eeb !@@ eta !@ {\"hello\"}) >> testneq {e2} (eeb !$ {e1})"
                    , testExpectSuccess
                          "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> removeAll (eeb !@@ eta !@ {\"hello\"}) >> testeq {\"hello\"} (eta !$ {e2})"
                    ]
              , tgroup
                    "single"
                    [ testExpectSuccess "testisunknown (single $ eib !$$ eia !@ {0})"
                    , testExpectSuccess
                          "eib !$ {e1} := 1 >> eia !$ {e1} := 0 >> testeq {1} (single $ eib !$$ eia !@ {0})"
                    , testExpectSuccess
                          "eib !$ {e1} := 1 >> eia !$ {e1} := 0 >> eic !$ {e1} := 0 >> testeq {1} (single $ eib !$$ eia !@ {0})"
                    , testExpectSuccess
                          "eib !$ {e1} := 1 >> eia !$ {e1} := 0 >> eia !$ {e1} := 0 >> testeq {1} (single $ eib !$$ eia !@ {0})"
                    , testExpectSuccess
                          "eib !$ {e1} := 1 >> eib !$ {e2} := 2 >> eia !$ {e1} := 0 >> eia !$ {e2} := 0 >> testisunknown (single $ eib !$$ eia !@ {0})"
                    , testExpectSuccess
                          "eib !$ {e1} := 1 >> eib !$ {e2} := 1 >> eia !$ {e1} := 0 >> eia !$ {e2} := 0 >> testeq {1} (single $ eib !$$ eia !@ {0})"
                    ]
              , tgroup
                    "multiple set member"
                    [ testExpectSuccess "testeq {0} (count (tea !@ {e1}))"
                    , testExpectSuccess "eea !$ {e2} := e1 >> testeq {1} (count (eea !@ {e1}))"
                    , testExpectSuccess "eea !@ {e1} += e2 >> testeq {1} (count (eea !@ {e1}))"
                    , testExpectSuccess "tea !$ {\"hello\"} := e1 >> testeq {e1} (tea !$ {\"hello\"})"
                    , testExpectSuccess "tea !@ {e1} += \"hello\" >> testeq {e1} (tea !$ {\"hello\"})"
                    , testExpectSuccess "tea !$ {\"hello\"} := e1 >> testeq {1} (count (tea !@ {e1}))"
                    , testExpectSuccess "tea !@ {e1} += \"hello\" >> testeq {1} (count (tea !@ {e1}))"
                    , testExpectSuccess
                          "tea !@ {e1} += \"hello\" >> tea !@ {e1} += \"hello\" >> testeq {1} (count (tea !@ {e1}))"
                    , testExpectSuccess
                          "tea !@ {e1} += \"h\" >> tea !@ {e1} += \"hello\" >> testeq {2} (count (tea !@ {e1}))"
                    , testExpectSuccess $
                      "let counter = eia !$ {e1};someset = nea !@ {e1} in " <>
                      "counter := 0 >> someset += 1 >> someset += 1 >> (get (members noOrder someset) >>= \\pp -> for pp $ \\p -> runWholeRef {counter := %counter + 1}) >> testeq {1} counter"
                    ]
              , tgroup
                    "types"
                    [ testExpectSuccess "let opentype T1; p = property @T1 @T1 !\"p\" in pass"
                    , testExpectSuccess "let opentype T1 in let p = property @T1 @T1 !\"p\" in pass"
                    , testExpectSuccess "let opentype T1; opentype T2; p = property @T1 @T2 !\"p\" in pass"
                    , testExpectSuccess "let opentype T1; opentype T2 in let p = property @T1 @T2 !\"p\" in pass"
                    , testExpectSuccess "let opentype T1 in let opentype T2; p = property @T1 @T2 !\"p\" in pass"
                    , testExpectSuccess "let opentype T1 in let opentype T2 in let p = property @T1 @T2 !\"p\" in pass"
                    , testExpectReject "let opentype T1 in let opentype T1 in pass"
                    , testExpectReject "let opentype T1; opentype T1 in pass"
                    ]
              , tgroup
                    "Maybe"
                    [ testExpectSuccess
                          "let enta = property @E @(Maybe Text) !\"enta\" in enta !$ {e1} := Just \"abc\" >> (testeq {Just \"abc\"} $ enta !$ {e1})"
                    , testExpectSuccess
                          "let enta = property @E @(Maybe Text) !\"enta\" in enta !$ {e1} := Nothing >> (testeq {Nothing} $ enta !$ {e1})"
                    ]
              , tgroup
                    "List"
                    [ testExpectSuccess
                          "let enta = property @E @[Text] !\"enta\" in enta !$ {e1} := [\"abc\", \"def\"] >> (testeq {[\"abc\", \"def\"]} $ enta !$ {e1})"
                    , testExpectSuccess
                          "let enta = property @E @[Text] !\"enta\" in enta !$ {e1} := [] >> (testeq {[]} $ enta !$ {e1})"
                    ]
              , tgroup
                    "Pair/Either"
                    [ testExpectSuccess
                          "let enta = property @E @(Number, Text) !\"enta\" in enta !$ {e1} := (74,\"hmm\") >> (testneq {(71,\"hmm\")} $ enta !$ {e1})"
                    , testExpectSuccess
                          "let enta = property @E @(Number, Text) !\"enta\" in enta !$ {e1} := (74,\"hmm\") >> (testeq {(74,\"hmm\")} $ enta !$ {e1})"
                    , testExpectSuccess
                          "let enta = property @E @(Either Number Text) !\"enta\" in enta !$ {e1} := Left 74 >> (testneq {Left 73} $ enta !$ {e1})"
                    , testExpectSuccess
                          "let enta = property @E @(Either Number Text) !\"enta\" in enta !$ {e1} := Left 74 >> (testeq {Left 74} $ enta !$ {e1})"
                    , testExpectSuccess
                          "let enta = property @E @(Either Number Text) !\"enta\" in enta !$ {e1} := Right \"abc\" >> (testneq {Right \"adbc\"} $ enta !$ {e1})"
                    , testExpectSuccess
                          "let enta = property @E @(Either Number Text) !\"enta\" in enta !$ {e1} := Right \"abc\" >> (testeq {Right \"abc\"} $ enta !$ {e1})"
                    ]
              ]
        , let
              subtypeTests p q =
                  [ testExpectSuccess "pass"
                  , testExpectSuccess $ "let f : " <> p <> " -> " <> q <> "; f x = x in pass"
                  , testExpectSuccess $ "let f : [" <> p <> "] -> [" <> q <> "]; f x = x in pass"
                  , testExpectReject $ "let f : " <> q <> " -> " <> p <> "; f x = x in pass"
                  ]
              equivalentTests p q =
                  [ testExpectSuccess "pass"
                  , testExpectSuccess $ "let f : " <> p <> " -> " <> q <> "; f x = x in pass"
                  , testExpectSuccess $ "let f : [" <> p <> "] -> [" <> q <> "]; f x = x in pass"
                  , testExpectSuccess $ "let f : " <> q <> " -> " <> p <> "; f x = x in pass"
                  , testExpectSuccess $ "let f : [" <> q <> "] -> [" <> p <> "]; f x = x in pass"
                  ]
              in tgroup
                     "subtype"
                     [ tgroup
                           "let"
                           [ context ["opentype P", "opentype Q", "subtype P <: Q"] $ tgroup "1" $ subtypeTests "P" "Q"
                           , context ["opentype P", "subtype P <: Q", "opentype Q"] $ tgroup "2" $ subtypeTests "P" "Q"
                           , context ["subtype P <: Q", "opentype P", "opentype Q"] $ tgroup "3" $ subtypeTests "P" "Q"
                           ]
                     , tgroup
                           "local"
                           [ context ["opentype P"] $
                             tgroup
                                 "1"
                                 [ testExpectSuccess "pass"
                                 , testExpectSuccess "let opentype Q; subtype P <: Q in pass"
                                 , testExpectSuccess "let opentype Q; subtype P <: Q; f : P -> Q; f x = x in pass"
                                 , testExpectReject "let opentype Q; subtype P <: Q; f : Q -> P; f x = x in pass"
                                 ]
                           , context ["opentype Q"] $
                             tgroup
                                 "2"
                                 [ testExpectSuccess "pass"
                                 , testExpectSuccess "let opentype P; subtype P <: Q in pass"
                                 , testExpectSuccess "let opentype P; subtype P <: Q; f : P -> Q; f x = x in pass"
                                 , testExpectReject "let opentype P; subtype P <: Q; f : Q -> P; f x = x in pass"
                                 ]
                           , context ["opentype P", "opentype Q"] $
                             tgroup
                                 "3"
                                 [ testExpectSuccess "pass"
                                 , testExpectSuccess "let subtype P <: Q in pass"
                                 , testExpectSuccess "let subtype P <: Q; f : P -> Q; f x = x in pass"
                                 , testExpectReject "let subtype P <: Q; f : Q -> P; f x = x in pass"
                                 ]
                           ]
                     , tgroup
                           "circular"
                           [ context ["opentype P", "subtype P <: P"] $
                             tgroup
                                 "single"
                                 [ testExpectSuccess "pass"
                                 , testExpectSuccess "let f : P -> P; f x = x in pass"
                                 , testExpectSuccess "let f : [P] -> [P]; f x = x in pass"
                                 ]
                           , context ["opentype P", "opentype Q", "subtype P <: Q", "subtype Q <: P"] $
                             tgroup
                                 "pair"
                                 [ testExpectSuccess "pass"
                                 , testExpectSuccess "let f : P -> P; f x = x in pass"
                                 , testExpectSuccess "let f : Q -> Q; f x = x in pass"
                                 , testExpectSuccess "let f : P -> Q; f x = x in pass"
                                 , testExpectSuccess "let f : [P] -> [Q]; f x = x in pass"
                                 , testExpectSuccess "let f : Q -> P; f x = x in pass"
                                 ]
                           ]
                     , context ["opentype Q", "subtype Maybe Number <: Q"] $
                       tgroup
                           "non-simple" -- not allowed, per issue #28
                           [testExpectReject "pass"]
                     , context ["opentype Q", "subtype Integer <: Q"] $ tgroup "literal" $ subtypeTests "Integer" "Q"
                     , context ["opentype Q", "closedtype P = P1 Text Number !\"P.P1\"", "subtype P <: Q"] $
                       tgroup "closed" $ subtypeTests "P" "Q"
                     , context
                           [ "opentype Q"
                           , "opentype R"
                           , "closedtype P = P1 Text Number !\"P.P1\""
                           , "subtype P <: Q"
                           , "subtype P <: R"
                           ] $
                       tgroup "closed" $ subtypeTests "P" "R"
                     , tgroup
                           "Entity"
                           [ testExpectSuccess "let f : Number -> Entity; f x = x in pass"
                           , testExpectSuccess "let f : (a & Number) -> (Entity,a); f x = (x,x) in pass"
                           , testExpectSuccess "let f : Maybe Number -> Entity; f x = x in pass"
                           , testExpectSuccess "let f : Maybe (a & Number) -> (Entity,Maybe a); f x = (x,x) in pass"
                           ]
                     , tgroup "dynamic" $
                       [ tgroup "DynamicEntity <: Entity" $ subtypeTests "DynamicEntity" "Entity"
                       , context ["dynamictype P1 = !\"P1\"", "dynamictype P2 = !\"P2\"", "dynamictype Q = P1 | P2"] $
                         tgroup "P1 <: DynamicEntity" $ subtypeTests "P1" "DynamicEntity"
                       , context ["dynamictype P1 = !\"P1\"", "dynamictype P2 = !\"P2\"", "dynamictype Q = P1 | P2"] $
                         tgroup "Q <: DynamicEntity" $ subtypeTests "Q" "DynamicEntity"
                       , context ["dynamictype P1 = !\"P1\"", "dynamictype P2 = !\"P2\"", "dynamictype Q = P1 | P2"] $
                         tgroup "P1 <: Entity" $ subtypeTests "P1" "Entity"
                       , context ["dynamictype P1 = !\"P1\"", "dynamictype P2 = !\"P2\"", "dynamictype Q = P1 | P2"] $
                         tgroup "Q <: Entity" $ subtypeTests "Q" "Entity"
                       , context ["dynamictype P1 = !\"P1\"", "dynamictype P2 = !\"P2\"", "dynamictype Q = P1 | P2"] $
                         tgroup "1" $ subtypeTests "P1" "Q"
                       , context ["dynamictype P1 = !\"P1\"", "dynamictype Q = P1 | P2", "dynamictype P2 = !\"P2\""] $
                         tgroup "2" $ subtypeTests "P1" "Q"
                       , context ["dynamictype Q = P1 | P2", "dynamictype P1 = !\"P1\"", "dynamictype P2 = !\"P2\""] $
                         tgroup "3" $ subtypeTests "P1" "Q"
                       , context
                             [ "opentype T"
                             , "subtype QA <: T"
                             , "dynamictype QA = P1 | P2 | P3"
                             , "dynamictype QB = P2 | P3 | P1"
                             , "dynamictype QC = P2 | P3"
                             , "dynamictype P1 = !\"P1\""
                             , "dynamictype P2 = !\"P2\""
                             , "dynamictype P3 = !\"P3\""
                             ] $
                         tgroup
                             "open-transitive"
                             [ tgroup "QC <: QB" $ subtypeTests "QC" "QB"
                             , tgroup "QA = QB" $ equivalentTests "QA" "QB"
                             , tgroup "QA <: T" $ subtypeTests "QA" "T"
                             , tgroup "QB <: T" $ subtypeTests "QB" "T"
                             , tgroup "QC <: T" $ subtypeTests "QC" "T"
                             , tgroup "P1 <: T" $ subtypeTests "P1" "T"
                             ]
                       , tgroup
                             "cycle"
                             [ context ["dynamictype P = P"] $ testExpectReject "pass"
                             , context ["dynamictype P = Q", "dynamictype Q = P"] $ testExpectReject "pass"
                             , context ["dynamictype P = Q", "dynamictype Q = P"] $
                               testExpectReject "let f: P -> Q; f x = x in pass"
                             , context ["dynamictype P1 = !\"P1\"", "dynamictype P = P1 | Q", "dynamictype Q = P"] $
                               testExpectReject "pass"
                             , context ["dynamictype P1 = !\"P1\"", "dynamictype P = P1 | Q", "dynamictype Q = P | Q"] $
                               testExpectReject "pass"
                             , context ["dynamictype P1 = !\"P1\"", "dynamictype Q = P1 | Q"] $ testExpectReject "pass"
                             ]
                       ]
                     ]
        , context
              [ "dynamictype P1 = !\"P1\""
              , "dynamictype P2 = !\"P2\""
              , "dynamictype Q = P1 | P2"
              , "e1 = dynamicEntity @P1 !\"e1\""
              ] $
          tgroup
              "dynamictype"
              [ testExpectSuccess "pass"
              , testExpectSuccess "testeq {e1} {e1}"
              , testExpectSuccess "testeq {Just e1} {check @P1 e1}"
              , testExpectSuccess "testeq {Nothing} {check @P2 e1}"
              , testExpectSuccess "testeq {Just e1} {check @Q e1}"
              , testExpectSuccess "testeq {True} {case e1 of (_: P1) -> True; _ -> False end}"
              , testExpectSuccess "testeq {False} {case e1 of (_: P2) -> True; _ -> False end}"
              , testExpectSuccess "testeq {True} {case e1 of (_: Q) -> True; _ -> False end}"
              , testExpectSuccess "testeq {e1} {coerce @P1 e1}"
              , testExpectSuccess "testeq {e1} {coerce @Q e1}"
              ]
        , context
              [ "datatype T = T1 Text Number | T2 | T3 Boolean | T4 (WholeRef {-Boolean,+Integer} -> Integer) | T5 Text (Boolean -> Integer)"
              ] $
          tgroup
              "datatype"
              [ testExpectSuccess "pass"
              , testExpectSuccess "let t1 = T1 \"hello\" 3 in pass"
              , testExpectSuccess "let f (T1 x _) = x in pass"
              , testExpectSuccess "case T2 of T2 -> pass end"
              , testExpectSuccess "case T3 True of T3 True -> pass end"
              , testExpectSuccess "case T1 \"hello\" 3 of T1 \"hello\" 3 -> pass end"
              , testExpectSuccess
                    "case T1 \"hello\" 3 of T2 -> fail \"T2\"; T1 \"hello\" 2 -> fail \"T1 2\"; T1 \"hell\" 3 -> fail \"T1 hell\"; T1 \"hello\" 3 -> pass end"
              , testExpectSuccess
                    "let f : Boolean -> Integer; f b = if b then 1 else 0 in case T5 \"abcd\" f of T5 _ ff -> if ff True == 1 then pass else fail \"ff\" end"
              , testExpectReject "let datatype B = MkB a in pass"
              , tmodify (ignoreTestBecause "ISSUE #41") $ testExpectSuccess "let datatype B a = MkB a in pass"
              , testExpectSuccess "let datatype P in pass"
              , tgroup
                    "nominal"
                    [ testExpectSuccess "let datatype P = P1; f : P -> P; f x = x in pass"
                    , testExpectReject "let datatype P = P1; datatype Q; f : P -> Q; f x = x in pass"
                    , testExpectReject "let datatype P; datatype Q = Q1; f : P -> Q; f x = x in pass"
                    , testExpectReject "let datatype P; datatype Q; f : P -> Q; f x = x in pass"
                    , testExpectReject "let datatype P = P1; datatype Q = Q1; f : P -> Q; f x = x in pass"
                    , testExpectReject
                          "let datatype P = P1 Integer; datatype Q = Q1 Integer; f : P -> Q; f x = x in pass"
                    ]
              , tgroup
                    "recursive"
                    [ testExpectSuccess "let datatype P = P1 in let datatype Q = Q1 P in pass"
                    , testExpectSuccess "let datatype P = P1; datatype Q = Q1 P in pass"
                    , testExpectSuccess "let datatype P = P1 Q; datatype Q in pass"
                    , testExpectSuccess "let datatype P = P1 Q; datatype Q = Q1 P in pass"
                    , testExpectSuccess "let datatype P = P1 P in pass"
                    , testExpectSuccess
                          "let datatype P = P1 Q; datatype Q = Q1 P; f : P -> P; f p = case p of P1 q -> case q of Q1 p -> p end end in pass"
                    , testExpectSuccess "let datatype P = P1 Q; closedtype Q = Q1 !\"Q1\" in pass"
                    , testExpectReject "let closedtype P = P1 Q; datatype Q = Q1 !\"Q1\" in pass"
                    , testExpectSuccess
                          "let datatype P = P1 Q; datatype Q = Q1 (Action ()); pqpass = P1 (Q1 pass) in case pqpass of P1 (Q1 p) -> p end"
                    ]
              ]
        , context ["closedtype T = T1 Text Number !\"T.T1\" | T2 !\"T.T2\" | T3 Boolean !\"T.T3\""] $
          tgroup
              "closedtype"
              [ testExpectSuccess "pass"
              , testExpectSuccess "let t1 = T1 \"hello\" 3 in pass"
              , testExpectSuccess "let f (T1 x _) = x in pass"
              , testExpectSuccess "case T1 \"hello\" 3 of T1 \"hello\" 3 -> pass end"
              , testExpectSuccess
                    "case T1 \"hello\" 3 of T2 -> fail \"T2\"; T1 \"hello\" 2 -> fail \"T1 2\"; T1 \"hell\" 3 -> fail \"T1 hell\"; T1 \"hello\" 3 -> pass end"
              , testExpectSuccess "let closedtype P in pass"
              , tgroup
                    "nominal"
                    [ testExpectSuccess "let closedtype P = P1 !\"P1\"; f : P -> P; f x = x in pass"
                    , testExpectReject "let closedtype P = P1 !\"P1\"; closedtype Q; f : P -> Q; f x = x in pass"
                    , testExpectReject "let closedtype P; closedtype Q = Q1 !\"Q1\"; f : P -> Q; f x = x in pass"
                    , testExpectReject "let closedtype P; closedtype Q; f : P -> Q; f x = x in pass"
                    , testExpectReject
                          "let closedtype P = P1 !\"P1\"; closedtype Q = Q1 !\"Q1\"; f : P -> Q; f x = x in pass"
                    , testExpectReject
                          "let closedtype P = P1 Integer !\"P1\"; closedtype Q = Q1 Integer !\"Q1\"; f : P -> Q; f x = x in pass"
                    ]
              , tgroup
                    "recursive"
                    [ testExpectSuccess "let closedtype P = P1 !\"P1\" in let closedtype Q = Q1 P !\"Q1\" in pass"
                    , testExpectSuccess "let closedtype P = P1 !\"P1\"; closedtype Q = Q1 P !\"Q1\" in pass"
                    , testExpectSuccess "let closedtype P = P1 Q !\"P1\"; closedtype Q in pass"
                    , testExpectSuccess "let closedtype P = P1 Q !\"P1\"; closedtype Q = Q1 P !\"Q1\" in pass"
                    , testExpectSuccess "let closedtype P = P1 P !\"P1\" in pass"
                    ]
              ]
        , tgroup
              "type escape"
              [ testExpectSuccess
                    "let opentype T; t = let in openEntity @T !\"t\"; f = let f : T -> Action (); f _ = pass in f; in f t"
              , testExpectReject
                    "let opentype T1; opentype T2; t = let in openEntity @T1 !\"t\"; f = let f : T2 -> Action (); f _ = pass in f; in f t"
              , testExpectReject
                    "let t = let opentype T in openEntity @T !\"t\"; f = let opentype T; f : T -> Action (); f _ = pass in f; in f t"
              , testExpectReject
                    "let t = let opentype T1 in openEntity @T1 !\"t\"; f = let opentype T2; f : T2 -> Action (); f _ = pass in f; in f t"
              ]
        , context ["opentype E", "eta = property @E @Text !\"eta\"", "e1 = openEntity @E !\"e1\"", "rt1 = eta !$ {e1}"] $
          tgroup
              "undo"
              [ testExpectSuccess "do rt1 := \"A\"; testeq {\"A\"} rt1; rt1 := \"B\"; testeq {\"B\"} rt1; end"
              , testExpectSuccess
                    "do rt1 := \"A\"; testeq {\"A\"} rt1; rt1 := \"B\"; testeq {\"B\"} rt1; queueUndo; testeq {\"A\"} rt1; end"
              , testExpectSuccess
                    "do rt1 := \"A\"; testeq {\"A\"} rt1; rt1 := \"B\"; testeq {\"B\"} rt1; queueUndo; testeq {\"A\"} rt1; queueRedo; testeq {\"B\"} rt1; end"
              ]
        , tgroup
              "interpret"
              [ testExpectSuccess "do r <- newMemWhole; interpretIntegerAsText r := \"37\"; testeq {37} r; end"
              , testExpectSuccess
                    "do r <- newMemWhole; interpretDateAsText r := \"2015-08-12\"; testeq {Date 2015 08 12} r; end"
              ]
        , context
              [ "runresult ar arg = case ar of Left err -> fail err; Right f -> f arg end"
              , "testaction expected action = do found <- action; testeqval expected found end"
              ] $
          tgroup
              "evaluate"
              [ testExpectSuccess "testaction (Right True) $ evaluate @Boolean \"True\""
              , testExpectSuccess "testaction (Right 5) $ evaluate @Integer \"5\""
              , testExpectSuccess "testaction (Right 5) $ evaluate @Integer \"let x = 5 in x\""
              , testExpectSuccess
                    "do ar <- evaluate @(Integer -> Integer) \"\\\\x -> x + 1\"; case ar of Left err -> fail err; Right f -> testeqval 8 $ f 7 end end"
              , testExpectSuccess "testaction (Left \"<evaluate>:1:1: expecting: expression\") $ evaluate @Integer \"\""
              , testExpectSuccess "testaction (Left \"<evaluate>:1:1: undefined: f:a\") $ evaluate @Integer \"f\""
              , testExpectSuccess
                    "testaction (Left \"<evaluate>:1:1: cannot convert Text <: Integer\\\n<evaluate>:1:1: cannot subsume Text <: Integer\") $ evaluate @Integer \"\\\"hello\\\"\""
              , testExpectSuccess
                    "do r <- newMemWhole; ar <- evaluate @(WholeRef Integer -> Action ()) \"\\\\r -> r := 45\"; runresult ar r; a <- get r; testeqval 45 a; end"
              ]
        ]
