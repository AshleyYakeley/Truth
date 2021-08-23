module Test.Entity
    ( testEntity
    , testUpdates
    ) where

import Changes.Core
import Pinafore
import Pinafore.Test
import Shapes
import Test.RunScript

testUpdate :: Text -> ScriptTestTree
testUpdate text =
    testExpression text text $ \cc interpret -> do
        (stuff :: PinaforeAction _) <- liftIO interpret
        (sendUpdate, ref) <- ccRunView cc emptyResourceContext $ unliftPinaforeActionOrFail stuff
        runEditor emptyResourceContext (unWModel $ immutableRefToRejectingRef ref) $
            checkUpdateEditor (Known (1 :: Integer)) $
            ccUnliftLifeCycle cc $ ccRunView cc emptyResourceContext $ unliftPinaforeActionOrFail sendUpdate

testUpdates :: TestTree
testUpdates = runScriptTestTree $ tGroup "update" [testUpdate "do ref <- newMemWhole; return (ref := 1, ref) end"]

data SubtypeResult
    = SRNot
    | SRUnify
    | SRSubsume
    | SRSingle

testEntity :: TestTree
testEntity =
    runScriptTestTree $
    tDecls
        [ "pass = return ()"
        , "undefined = error \"undefined\""
        , "runWholeRef r = do a <- get r; a end"
        , "runreforfail r = runWholeRef (r ?? {fail \"unknown ref\"})"
        , "testeq expected found = runreforfail {if %expected == %found then pass else fail \"not equal\"}"
        , "testneq expected found = runreforfail {if %expected /= %found then pass else fail \"equal\"}"
        , "testisknown t = runWholeRef {if %(known t) then pass else fail \"known\"}"
        , "testisunknown t = runWholeRef {if %(known t) then fail \"known\" else pass}"
        , "testeqval e f = testeq {e} {f}"
        , "expectStop p = onStop (p >> fail \"no stop\") pass"
        ] $
    tGroup
        "entity"
        [ tDecls [] $
          tGroup
              "current" -- stack test pinafore --test-arguments "--pattern entity.current"
              []
        , tGroup
              "pass"
              [ testExpectSuccess "pass"
              , testExpectSuccess "pass >> pass"
              , testExpectSuccess "if True then pass else fail \"failed\""
              , testExpectSuccess "pass >> if True then pass else fail \"failed\""
              ]
        , tGroup
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
        , tGroup
              "fail"
              [ testExpectStop "stop"
              , testExpectThrow "fail \"text\""
              , testExpectThrow "let in fail \"text\""
              , testExpectThrow "let t = 1 in fail \"text\""
              , testExpectThrow "let opentype T in fail \"text\""
              ]
        , tGroup
              "do"
              [ testExpectSuccess "do return () end"
              , testExpectSuccess "do return (); end"
              , testExpectSuccess "do testeqval 3 3 end"
              , testExpectSuccess "do a <- return 3; testeqval 3 a end"
              , testExpectSuccess "do a <- return 3; b <- return $ a + a; testeqval 6 b end"
              ]
        , tDecls ["flagRef = do r <- newMemWhole; r := False; return r; end"] $
          tGroup
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
        , tGroup
              "equality"
              [ testExpectSuccess "testeqval 1 1"
              , testExpectSuccess "testeqval False $ 1 == \"1\""
              , testExpectSuccess "testeqval False $ 0 == 1"
              , testExpectSuccess "testeqval True $ 1 == 1"
              , testExpectSuccess "testeqval False $ 1 == ~1"
              ]
        , tGroup
              "reference notation"
              [ testExpectSuccess "runreforfail {pass}"
              , testExpectSuccess "let p = pass in runreforfail {p}"
              , testExpectSuccess "runreforfail {let p = pass in p}"
              , testExpectSuccess "runreforfail {%{pass}}"
              , testExpectSuccess "let rp = {pass} in runreforfail {%rp}"
              , testExpectSuccess "runreforfail {let rp = {pass} in %rp}"
              , testExpectSuccess "let rp = {pass} in runreforfail {let p= %rp in p}"
              ]
        , tGroup
              "reference stop"
              [ testExpectSuccess "expectStop $ stop"
              , testExpectSuccess "expectStop $ get unknown"
              , testExpectSuccess "expectStop $ {1} := 1"
              , testExpectSuccess "expectStop $ delete {1}"
              ]
        , tGroup
              "memory references"
              [ testExpectSuccess "expectStop $ do r <- newMemWhole; get r; end"
              , testExpectSuccess "do r <- newMemWhole; r := 45; a <- get r; testeqval 45 a; end"
              , testExpectSuccess "do r <- newMemWhole; r := 3; r := 4; a <- get r; testeqval 4 a; end"
              , testExpectSuccess "do s <- newMemFiniteSet; n <- get $ setCount s; testeqval 0 n; end"
              , testExpectSuccess "do s <- newMemFiniteSet; s += 57; n <- get $ setCount s; testeqval 1 n; end"
              , testExpectSuccess "do s <- newMemFiniteSet; s -= 57; n <- get $ setCount s; testeqval 0 n; end"
              , testExpectSuccess "do s <- newMemFiniteSet; s += 57; s -= 57; n <- get $ setCount s; testeqval 0 n; end"
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
        , tDecls
              [ "showVal: Showable -> Action (); showVal v = Debug.message $ show v"
              , "showList: [Showable] -> Action (); showList l = do Debug.message \"[[[\"; for_ l showVal;  Debug.message \"]]]\"; end"
              , mif False "testImmutList: Boolean -> Integer -> (ListRef Integer -> Action ()) -> Action ();" <>
                "testImmutList present n call = do lr <- newMemWhole; lr := [10,20,30]; r <- listGetItemRef present n lr; ir <- listGetItemRef present n $ immutWhole lr; call lr; a <- get r; ia <- get ir; testeqval a ia; end"
              ] $
          tGroup
              "list references"
              [ testExpectSuccess "do r <- newMemList; n <- listGetCount r; testeqval 0 n; end"
              , testExpectSuccess
                    "do r <- newMemList; listWhole r := [10,20,30]; n <- listGetCount r; testeqval 3 n; end"
              , testExpectSuccess "do r <- newMemList; n <- get $ listCountRef r; testeqval 0 n; end"
              , testExpectSuccess
                    "do r <- newMemList; listWhole r := [10,20,30]; n <- get $ listCountRef r; testeqval 3 n; end"
              , testExpectSuccess
                    "do r <- newMemList; listWhole r := [10,20,30]; ir <- listGetItemRef True 1 r; i <- get ir; testeqval 20 i; end"
              , testExpectSuccess
                    "do r <- newMemList; listWhole r := [10,20,30]; ir <- listGetItemRef True 1 r; ir := 25; i <- get ir; testeqval 25 i; end"
              , testExpectSuccess
                    "do r <- newMemList; listWhole r := [10,20,30]; ir <- listGetItemRef True 1 r; ir := 25; l <- get $ listWhole r; testeqval [10,25,30] l; end"
              , testExpectSuccess
                    "do r <- newMemList; listWhole r := [10,20,30]; ir <- listGetItemRef True 1 r; delete ir; l <- get $ listWhole r; testeqval [10,30] l; end"
              , testExpectSuccess
                    "do r <- newMemList; listWhole r := [10,20,30]; ir <- listGetItemRef True 1 r; delete ir; ir := 15; l <- get $ listWhole r; testeqval [10,15,30] l; end"
              , testExpectSuccess
                    "do r <- newMemList; listWhole r := [10,20,30]; ir <- listGetItemRef False 1 r; i <- expectStop $ get ir; return (); end"
              , testExpectSuccess
                    "do r <- newMemList; listWhole r := [10,20,30]; ir <- listGetItemRef False 1 r; ir := 25; i <- get ir; testeqval 25 i; end"
              , testExpectSuccess
                    "do r <- newMemList; listWhole r := [10,20,30]; ir <- listGetItemRef False 1 r; ir := 25; l <- get $ listWhole r; testeqval [10,25,20,30] l; end"
              , testExpectSuccess
                    "do r <- newMemList; listWhole r := [10,20,30]; ir <- listGetItemRef False 1 r; delete ir; l <- get $ listWhole r; testeqval [10,20,30] l; end"
              , testExpectSuccess
                    "do r <- newMemList; listWhole r := [10,20,30]; ir <- listGetItemRef False 1 r; delete ir; ir := 15; l <- get $ listWhole r; testeqval [10,15,20,30] l; end"
              , testExpectSuccess
                    "do r <- newMemList; listWhole r := [10,20,30]; ir <- listGetItemRef False 1 r; delete ir; l <- get $ listWhole r; testeqval [10,20,30] l; end"
              , testExpectSuccess
                    "do r <- newMemList; listWhole r := [10,20,30]; ir <- listGetItemRef False 1 r; delete ir; ir := 15; l <- get $ listWhole r; testeqval [10,15,20,30] l; end"
              , testExpectSuccess
                    "do r <- newMemList; listWhole r := [10,20,30]; ir <- listGetItemRef True 1 r; listInsert 1 12 r; i <- get ir; testeqval 20 i; end"
              , testExpectSuccess
                    "do r <- newMemList; listWhole r := [10,20,30]; ir <- listGetItemRef True 1 r; listInsert 1 12 r; ir := 15; l <- get $ listWhole r; testeqval [10,12,15,30] l; end"
              , testExpectSuccess "testImmutList True 1 $ \\_ -> return ()"
              ]
        , tDecls
              [ "convr : Rational -> Rational;convr = id"
              , "convn : Number -> Number;convn = id"
              , "convl : Literal -> Literal;convl = id"
              , "testconvr : Rational -> Action ();testconvr r = testeq {convl r} {convl $ convn r}"
              ] $
          tGroup
              "literal conversion"
              [ tGroup
                    "Rational to Number"
                    [ testExpectSuccess "testconvr 1"
                    , testExpectSuccess "testconvr 2.5"
                    , testExpectSuccess "testeq {convl 31.5} {convl $ convn 31.5}"
                    , testExpectSuccess "testeq {\"63/2\"} {show 31.5}"
                    , testExpectSuccess "testeq {\"63/2\"} {show $ convn 31.5}"
                    ]
              ]
        , tDecls
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
          tGroup
              "Storage"
              [ tGroup
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
              , tGroup
                    ":="
                    [ testExpectSuccess "eta !$ {e1} := \"hello\""
                    , testExpectSuccess "eea !$ {e1} := e2"
                    , testExpectSuccess "eea !$ {e1} := e2 >> testeq {e2} (eea !$ {e1})"
                    , testExpectSuccess "eta !$ {e1} := \"hello\" >> testeq {\"hello\"} (eta !$ {e1})"
                    , testExpectSuccess "tea !$ {\"hello\"} := e1 >> testeq {e1} (tea !$ {\"hello\"})"
                    , testExpectSuccess "tea !$ {\"hello\"} := e1 >> testeq {1} (setCount (tea !@ {e1}))"
                    , testExpectSuccess "(eea !. eea) !$ {e1} := e2"
                    , testExpectSuccess
                          "do (eea !. eea) !$ {e1} := e2; testeq {e2} ((eea !. eea) !$ {e1}); testeq {e2} (eea !$ (eea !$ {e1})); end"
                    , testExpectSuccess
                          "do eea !$ (eea !$ {e1}) := e2; testeq {e2} ((eea !. eea) !$ {e1}); testeq {e2} (eea !$ (eea !$ {e1})); end"
                    , testExpectSuccess "expectStop $ do r <- newMemWhole; eia !$ r := 4; end"
                    ]
              , tGroup
                    "+="
                    [ testExpectSuccess "eta !@ {\"hello\"} += e1"
                    , testExpectSuccess "eta !@ {\"hello\"} += e1 >> pass"
                    , testExpectSuccess "eta !@ {\"hello\"} += e1 >> testeq {\"hello\"} (eta !$ {e1})"
                    ]
              , tGroup
                    "-="
                    [ testExpectSuccess
                          "eta !@ {\"hello\"} += e1 >> eta !@ {\"hello\"} -= e1 >> testisunknown (eta !$ {e1})"
                    ]
              , tGroup
                    "setClear"
                    [ testExpectSuccess
                          "eta !@ {\"hello\"} += e1 >> setClear (eta !@ {\"hello\"}) >> testisunknown (eta !$ {e1})"
                    ]
              , tGroup
                    "literal storage"
                    [ tGroup
                          "Boolean"
                          [ testExpectSuccess "eba !$ {e1} := True >> testeq {True} (eba !$ {e1})"
                          , testExpectSuccess "eba !$ {e1} := False >> testeq {False} (eba !$ {e1})"
                          ]
                    , tGroup
                          "Text"
                          [ testExpectSuccess "eta !$ {e1} := \"\" >> testeq {\"\"} (eta !$ {e1})"
                          , testExpectSuccess "eta !$ {e1} := \"hello\" >> testeq {\"hello\"} (eta !$ {e1})"
                          ]
                    , tGroup
                          "Integer"
                          [ testExpectSuccess "eia !$ {e1} := 0 >> testeq {0} (eia !$ {e1})"
                          , testExpectSuccess "eia !$ {e1} := 47 >> testeq {47} (eia !$ {e1})"
                          , testExpectSuccess "eia !$ {e1} := -12 >> testeq {-12} (eia !$ {e1})"
                          ]
                    , tGroup
                          "Rational"
                          [ testExpectSuccess "era !$ {e1} := 0 >> testeq {0} (era !$ {e1})"
                          , testExpectSuccess "era !$ {e1} := 47 >> testeq {47} (era !$ {e1})"
                          , testExpectSuccess "era !$ {e1} := -12 >> testeq {-12} (era !$ {e1})"
                          , testExpectSuccess "era !$ {e1} := 31.5 >> testeq {31.5} (era !$ {e1})"
                          , testExpectSuccess "era !$ {e1} := -22.8_70 >> testeq {-22.8_70} (era !$ {e1})"
                          ]
                    , tGroup
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
              , tGroup
                    "matching literals"
                    [ testExpectSuccess
                          "eta !$ {e1} := \"hello\" >> eta !$ {e2} := \"hello\" >> testeq (eta !$ {e1}) (eta !$ {e2})"
                    ]
              , tGroup
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
              , tGroup
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
              , tGroup
                    "composed morphisms"
                    [ testExpectSuccess "(eea !$ eeb !$ {e1}) := e2 >> testeq {e2} (eea !$ eeb !$ {e1})"
                    , testExpectSuccess "(eta !$ eeb !$ {e1}) := \"hello\" >> testeq {\"hello\"} (eta !$ eeb !$ {e1})"
                    , testExpectSuccess "(eea !. eeb !$ {e1}) := e2 >> testeq {e2} (eea !$ eeb !$ {e1})"
                    , testExpectSuccess "(eta !. eeb !$ {e1}) := \"hello\" >> testeq {\"hello\"} (eta !$ eeb !$ {e1})"
                    , testExpectSuccess "(eea !$ eeb !$ {e1}) := e2 >> testeq {e2} (eea !. eeb !$ {e1})"
                    , testExpectSuccess "(eta !$ eeb !$ {e1}) := \"hello\" >> testeq {\"hello\"} (eta !. eeb !$ {e1})"
                    , testExpectSuccess "(eeb !. eea) !$ {e2} := e1 >> testeq {e1} (eeb !$ eea !$ {e2})"
                    ]
              , tGroup
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
                          "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> setClear ((eta !. eeb) !@ {\"hello\"}) >> testeq {e2} (eeb !$ {e1})"
                    , testExpectSuccess
                          "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> setClear ((eta !. eeb) !@ {\"hello\"}) >> testisunknown (eta !$ {e2})"
                    , testExpectSuccess
                          "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> setClear (eeb !@@ eta !@ {\"hello\"}) >> testneq {e2} (eeb !$ {e1})"
                    , testExpectSuccess
                          "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> setClear (eeb !@@ eta !@ {\"hello\"}) >> testeq {\"hello\"} (eta !$ {e2})"
                    ]
              , tGroup
                    "setSingle"
                    [ testExpectSuccess "testisunknown (setSingle $ eib !$$ eia !@ {0})"
                    , testExpectSuccess
                          "eib !$ {e1} := 1 >> eia !$ {e1} := 0 >> testeq {1} (setSingle $ eib !$$ eia !@ {0})"
                    , testExpectSuccess
                          "eib !$ {e1} := 1 >> eia !$ {e1} := 0 >> eic !$ {e1} := 0 >> testeq {1} (setSingle $ eib !$$ eia !@ {0})"
                    , testExpectSuccess
                          "eib !$ {e1} := 1 >> eia !$ {e1} := 0 >> eia !$ {e1} := 0 >> testeq {1} (setSingle $ eib !$$ eia !@ {0})"
                    , testExpectSuccess
                          "eib !$ {e1} := 1 >> eib !$ {e2} := 2 >> eia !$ {e1} := 0 >> eia !$ {e2} := 0 >> testisunknown (setSingle $ eib !$$ eia !@ {0})"
                    , testExpectSuccess
                          "eib !$ {e1} := 1 >> eib !$ {e2} := 1 >> eia !$ {e1} := 0 >> eia !$ {e2} := 0 >> testeq {1} (setSingle $ eib !$$ eia !@ {0})"
                    ]
              , tGroup
                    "multiple set member"
                    [ testExpectSuccess "testeq {0} (setCount (tea !@ {e1}))"
                    , testExpectSuccess "eea !$ {e2} := e1 >> testeq {1} (setCount (eea !@ {e1}))"
                    , testExpectSuccess "eea !@ {e1} += e2 >> testeq {1} (setCount (eea !@ {e1}))"
                    , testExpectSuccess "tea !$ {\"hello\"} := e1 >> testeq {e1} (tea !$ {\"hello\"})"
                    , testExpectSuccess "tea !@ {e1} += \"hello\" >> testeq {e1} (tea !$ {\"hello\"})"
                    , testExpectSuccess "tea !$ {\"hello\"} := e1 >> testeq {1} (setCount (tea !@ {e1}))"
                    , testExpectSuccess "tea !@ {e1} += \"hello\" >> testeq {1} (setCount (tea !@ {e1}))"
                    , testExpectSuccess
                          "tea !@ {e1} += \"hello\" >> tea !@ {e1} += \"hello\" >> testeq {1} (setCount (tea !@ {e1}))"
                    , testExpectSuccess
                          "tea !@ {e1} += \"h\" >> tea !@ {e1} += \"hello\" >> testeq {2} (setCount (tea !@ {e1}))"
                    , testExpectSuccess $
                      "let counter = eia !$ {e1};someset = nea !@ {e1} in " <>
                      "counter := 0 >> someset += 1 >> someset += 1 >> (get (setList noOrder someset) >>= \\pp -> for pp $ \\p -> runWholeRef {counter := %counter + 1}) >> testeq {1} counter"
                    ]
              , tGroup
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
              , tGroup
                    "Maybe"
                    [ testExpectSuccess
                          "let enta = property @E @(Maybe Text) !\"enta\" in enta !$ {e1} := Just \"abc\" >> (testeq {Just \"abc\"} $ enta !$ {e1})"
                    , testExpectSuccess
                          "let enta = property @E @(Maybe Text) !\"enta\" in enta !$ {e1} := Nothing >> (testeq {Nothing} $ enta !$ {e1})"
                    ]
              , tGroup
                    "List"
                    [ testExpectSuccess
                          "let enta = property @E @[Text] !\"enta\" in enta !$ {e1} := [\"abc\", \"def\"] >> (testeq {[\"abc\", \"def\"]} $ enta !$ {e1})"
                    , testExpectSuccess
                          "let enta = property @E @[Text] !\"enta\" in enta !$ {e1} := [] >> (testeq {[]} $ enta !$ {e1})"
                    ]
              , tGroup
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
              testSubtypeUnify :: SubtypeResult -> Text -> ScriptTestTree
              testSubtypeUnify SRNot = testExpectReject
              testSubtypeUnify SRUnify = testExpectSuccess
              testSubtypeUnify SRSubsume = testExpectSuccess
              testSubtypeUnify SRSingle = testExpectSuccess
              testSubtypeSubsume :: SubtypeResult -> Text -> ScriptTestTree
              testSubtypeSubsume SRNot = testExpectReject
              testSubtypeSubsume SRUnify = testExpectReject
              testSubtypeSubsume SRSubsume = testExpectSuccess
              testSubtypeSubsume SRSingle = testExpectSuccess
              testSubtypeSingle :: SubtypeResult -> Text -> ScriptTestTree
              testSubtypeSingle SRNot = testExpectReject
              testSubtypeSingle SRUnify = testExpectReject
              testSubtypeSingle SRSubsume = testExpectReject
              testSubtypeSingle SRSingle = testExpectSuccess
              subtypeTests :: Bool -> SubtypeResult -> Text -> Text -> [ScriptTestTree]
              subtypeTests polar sr p q =
                  [ testExpectSuccess "pass"
                  , tGroup
                        "unify"
                        [ testSubtypeUnify sr $
                          "let rec f: (" <> q <> ") -> (); f = f end; rec x: " <> p <> "; x = x; end; fx = f x in pass"
                        ]
                  , tGroup "subsume" $
                    [ testSubtypeSubsume sr $ "let rec x: " <> p <> "; x = x end; y: " <> q <> "; y = x in pass"
                    , testSubtypeSubsume sr $ "let x: " <> p <> "; x = undefined; y: " <> q <> "; y = x in pass"
                    , testSubtypeSubsume sr $ "let x: " <> q <> "; x = undefined: " <> p <> " in pass"
                    , testSubtypeSubsume sr $ "let x = (undefined: " <> p <> "): " <> q <> " in pass"
                    , (if polar
                           then testExpectReject
                           else testSubtypeSingle sr) $
                      "let f: (" <> p <> ") -> (" <> q <> "); f x = x in pass"
                    ]
                  ]
              subtypeTest :: Bool -> SubtypeResult -> Text -> Text -> ScriptTestTree
              subtypeTest polar sr p q = tGroup (unpack $ p <> " <: " <> q) $ subtypeTests polar sr p q
              in tGroup
                     "subtype"
                     [ tGroup
                           "conversion"
                           [ subtypeTest False SRSingle "Integer" "Integer"
                           , subtypeTest False SRSingle "Integer" "Rational"
                           , subtypeTest False SRNot "Rational" "Integer"
                           , subtypeTest False SRSingle "Maybe Entity" "Entity"
                           , subtypeTest False SRNot "Entity" "Maybe Entity"
                           , subtypeTest False SRSingle "a" "a"
                           , subtypeTest False SRSubsume "a" "Integer"
                           , subtypeTest False SRUnify "Integer" "a"
                           , subtypeTest False SRSubsume "[a]" "[Integer]"
                           , subtypeTest False SRUnify "[Integer]" "[a]"
                           , subtypeTest False SRSubsume "(a,b)" "(Integer,Rational)"
                           , subtypeTest False SRSubsume "(a,a)" "(Integer,Integer)"
                           , subtypeTest False SRSubsume "(a,a)" "(Integer,Rational)"
                           , subtypeTest False SRSubsume "(a,Text)" "(Integer,Text)"
                           , subtypeTest False SRSingle "[a]" "[a]"
                           , subtypeTest False SRSingle "a -> a -> Ordering" "RefOrder a"
                           , subtypeTest False SRSingle "Integer -> Integer -> Ordering" "RefOrder Integer"
                           , subtypeTest False SRSingle "WholeRef [a]" "ListRef a"
                           , subtypeTest False SRSingle "WholeRef [Integer]" "ListRef Integer"
                           , subtypeTest False SRSingle "WholeRef {-[Integer],+[Integer]}" "ListRef Integer"
                           , subtypeTest True SRSingle "WholeRef {-[a & Integer],+[a | Integer]}" "ListRef Integer"
                           , subtypeTest True SRSingle "WholeRef {-[a & Entity],+[a | Integer]}" "ListRef Integer"
                           , subtypeTest False SRSingle "rec a. Maybe a" "rec a. Maybe a"
                           , subtypeTest False SRSingle "rec a. Maybe a" "rec b. Maybe b"
                           , subtypeTest False SRSingle "rec a. Maybe a" "Maybe (rec a. Maybe a)"
                           , subtypeTest False SRSingle "rec a. Maybe a" "Maybe (rec b. Maybe b)"
                           , subtypeTest False SRSingle "Maybe (rec a. Maybe a)" "rec a. Maybe a"
                           , subtypeTest False SRSingle "Maybe (rec a. Maybe a)" "rec b. Maybe b"
                           , subtypeTest False SRSingle "Maybe (rec a. Maybe a)" "Maybe (rec a. Maybe a)"
                           , subtypeTest False SRSingle "Maybe (rec a. Maybe a)" "Maybe (rec b. Maybe b)"
                           ]
                     , tGroup
                           "let"
                           [ tDecls ["opentype P", "opentype Q", "subtype P <: Q"] $
                             tGroup "seq" $ subtypeTests False SRSingle "P" "Q"
                           , tDeclsRec ["opentype P", "opentype Q", "subtype P <: Q"] $
                             tGroup "rec 1" $ subtypeTests False SRSingle "P" "Q"
                           , tDeclsRec ["opentype P", "subtype P <: Q", "opentype Q"] $
                             tGroup "rec 2" $ subtypeTests False SRSingle "P" "Q"
                           , tDeclsRec ["subtype P <: Q", "opentype P", "opentype Q"] $
                             tGroup "rec 3" $ subtypeTests False SRSingle "P" "Q"
                           ]
                     , tGroup
                           "local"
                           [ tDecls ["opentype P"] $
                             tGroup
                                 "1"
                                 [ testExpectSuccess "pass"
                                 , testExpectSuccess "let opentype Q; subtype P <: Q in pass"
                                 , testExpectSuccess "let opentype Q; subtype P <: Q; f : P -> Q; f x = x in pass"
                                 , testExpectReject "let opentype Q; subtype P <: Q; f : Q -> P; f x = x in pass"
                                 ]
                           , tDecls ["opentype Q"] $
                             tGroup
                                 "2"
                                 [ testExpectSuccess "pass"
                                 , testExpectSuccess "let opentype P; subtype P <: Q in pass"
                                 , testExpectSuccess "let opentype P; subtype P <: Q; f : P -> Q; f x = x in pass"
                                 , testExpectReject "let opentype P; subtype P <: Q; f : Q -> P; f x = x in pass"
                                 ]
                           , tDecls ["opentype P", "opentype Q"] $
                             tGroup
                                 "3"
                                 [ testExpectSuccess "pass"
                                 , testExpectSuccess "let subtype P <: Q in pass"
                                 , testExpectSuccess "let subtype P <: Q; f : P -> Q; f x = x in pass"
                                 , testExpectReject "let subtype P <: Q; f : Q -> P; f x = x in pass"
                                 ]
                           ]
                     , tGroup
                           "circular"
                           [ tDecls ["opentype P", "subtype P <: P"] $
                             tGroup
                                 "setSingle"
                                 [ testExpectSuccess "pass"
                                 , testExpectSuccess "let f : P -> P; f x = x in pass"
                                 , testExpectSuccess "let f : [P] -> [P]; f x = x in pass"
                                 ]
                           , tDecls ["opentype P", "opentype Q", "subtype P <: Q", "subtype Q <: P"] $
                             tGroup
                                 "pair"
                                 [ testExpectSuccess "pass"
                                 , testExpectSuccess "let f : P -> P; f x = x in pass"
                                 , testExpectSuccess "let f : Q -> Q; f x = x in pass"
                                 , testExpectSuccess "let f : P -> Q; f x = x in pass"
                                 , testExpectSuccess "let f : [P] -> [Q]; f x = x in pass"
                                 , testExpectSuccess "let f : Q -> P; f x = x in pass"
                                 ]
                           ]
                     , tDecls ["opentype Q", "subtype Maybe Number <: Q"] $
                       tGroup
                           "non-simple" -- not allowed, per issue #28
                           [testExpectReject "pass"]
                     , tDecls ["opentype Q", "subtype Integer <: Q"] $
                       tGroup "literal" $ subtypeTests False SRSingle "Integer" "Q"
                     , tDecls ["opentype Q", "closedtype P = P1 Text Number !\"P.P1\"", "subtype P <: Q"] $
                       tGroup "closed" $ subtypeTests False SRSingle "P" "Q"
                     , tDecls
                           [ "opentype Q"
                           , "opentype R"
                           , "closedtype P = P1 Text Number !\"P.P1\""
                           , "subtype P <: Q"
                           , "subtype P <: R"
                           ] $
                       tGroup "closed" $ subtypeTests False SRSingle "P" "R"
                     , tGroup
                           "Entity"
                           [ testExpectSuccess "let f : Number -> Entity; f x = x in pass"
                           , testExpectSuccess "let f : (a & Number) -> (Entity,a); f x = (x,x) in pass"
                           , testExpectSuccess "let f : Maybe Number -> Entity; f x = x in pass"
                           , testExpectSuccess "let f : Maybe (a & Number) -> (Entity,Maybe a); f x = (x,x) in pass"
                           ]
                     , tGroup "dynamic" $
                       [ tGroup "DynamicEntity <: Entity" $ subtypeTests False SRSingle "DynamicEntity" "Entity"
                       , tDecls ["dynamictype P1 = !\"P1\"", "dynamictype P2 = !\"P2\"", "dynamictype Q = P1 | P2"] $
                         tGroup "P1 <: DynamicEntity" $ subtypeTests False SRSingle "P1" "DynamicEntity"
                       , tDecls ["dynamictype P1 = !\"P1\"", "dynamictype P2 = !\"P2\"", "dynamictype Q = P1 | P2"] $
                         tGroup "Q <: DynamicEntity" $ subtypeTests False SRSingle "Q" "DynamicEntity"
                       , tDecls ["dynamictype P1 = !\"P1\"", "dynamictype P2 = !\"P2\"", "dynamictype Q = P1 | P2"] $
                         tGroup "P1 <: Entity" $ subtypeTests False SRSingle "P1" "Entity"
                       , tDecls ["dynamictype P1 = !\"P1\"", "dynamictype P2 = !\"P2\"", "dynamictype Q = P1 | P2"] $
                         tGroup "Q <: Entity" $ subtypeTests False SRSingle "Q" "Entity"
                       , tDecls ["dynamictype P1 = !\"P1\"", "dynamictype P2 = !\"P2\"", "dynamictype Q = P1 | P2"] $
                         tGroup "seq" $ subtypeTests False SRSingle "P1" "Q"
                       , tDeclsRec ["dynamictype P1 = !\"P1\"", "dynamictype P2 = !\"P2\"", "dynamictype Q = P1 | P2"] $
                         tGroup "rec 1" $ subtypeTests False SRSingle "P1" "Q"
                       , tDeclsRec ["dynamictype P1 = !\"P1\"", "dynamictype Q = P1 | P2", "dynamictype P2 = !\"P2\""] $
                         tGroup "rec 2" $ subtypeTests False SRSingle "P1" "Q"
                       , tDeclsRec ["dynamictype Q = P1 | P2", "dynamictype P1 = !\"P1\"", "dynamictype P2 = !\"P2\""] $
                         tGroup "rec 3" $ subtypeTests False SRSingle "P1" "Q"
                       , tDeclsRec
                             [ "opentype T"
                             , "subtype QA <: T"
                             , "dynamictype QA = P1 | P2 | P3"
                             , "dynamictype QB = P2 | P3 | P1"
                             , "dynamictype QC = P2 | P3"
                             , "dynamictype P1 = !\"P1\""
                             , "dynamictype P2 = !\"P2\""
                             , "dynamictype P3 = !\"P3\""
                             ] $
                         tGroup
                             "open-transitive"
                             [ tGroup "QC <: QB" $ subtypeTests False SRSingle "QC" "QB"
                             , tGroup "QA = QB" $ subtypeTests False SRSingle "QA" "QB"
                             , tGroup "QA = QB" $ subtypeTests False SRSingle "QB" "QA"
                             , tGroup "QA <: T" $ subtypeTests False SRSingle "QA" "T"
                             , tGroup "QB <: T" $ subtypeTests False SRSingle "QB" "T"
                             , tGroup "QC <: T" $ subtypeTests False SRSingle "QC" "T"
                             , tGroup "P1 <: T" $ subtypeTests False SRSingle "P1" "T"
                             ]
                       , tGroup
                             "cycle"
                             [ tDecls ["dynamictype P = P"] $ testExpectReject "pass"
                             , tDecls ["dynamictype P = Q", "dynamictype Q = P"] $ testExpectReject "pass"
                             , tDecls ["dynamictype P = Q", "dynamictype Q = P"] $
                               testExpectReject "let f: P -> Q; f x = x in pass"
                             , tDecls ["dynamictype P1 = !\"P1\"", "dynamictype P = P1 | Q", "dynamictype Q = P"] $
                               testExpectReject "pass"
                             , tDecls ["dynamictype P1 = !\"P1\"", "dynamictype P = P1 | Q", "dynamictype Q = P | Q"] $
                               testExpectReject "pass"
                             , tDecls ["dynamictype P1 = !\"P1\"", "dynamictype Q = P1 | Q"] $ testExpectReject "pass"
                             ]
                       ]
                     ]
        , tDecls
              [ "dynamictype P1 = !\"P1\""
              , "dynamictype P2 = !\"P2\""
              , "dynamictype Q = P1 | P2"
              , "e1 = dynamicEntity @P1 !\"e1\""
              ] $
          tGroup
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
        , tDecls
              [ "datatype T = T1 Text Number | T2 | T3 Boolean | T4 (WholeRef {-Boolean,+Integer} -> Integer) | T5 Text (Boolean -> Integer)"
              ] $
          tGroup
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
              , tModify (ignoreTestBecause "ISSUE #41") $ testExpectSuccess "let datatype B a = MkB a in pass"
              , testExpectSuccess "let datatype P in pass"
              , tGroup
                    "nominal"
                    [ testExpectSuccess "let datatype P = P1; f : P -> P; f x = x in pass"
                    , testExpectReject "let datatype P = P1; datatype Q; f : P -> Q; f x = x in pass"
                    , testExpectReject "let datatype P; datatype Q = Q1; f : P -> Q; f x = x in pass"
                    , testExpectReject "let datatype P; datatype Q; f : P -> Q; f x = x in pass"
                    , testExpectReject "let datatype P = P1; datatype Q = Q1; f : P -> Q; f x = x in pass"
                    , testExpectReject
                          "let datatype P = P1 Integer; datatype Q = Q1 Integer; f : P -> Q; f x = x in pass"
                    ]
              , tGroup
                    "recursive"
                    [ testExpectSuccess "let datatype P = P1 in let datatype Q = Q1 P in pass"
                    , testExpectSuccess "let datatype P = P1; datatype Q = Q1 P in pass"
                    , testExpectSuccess "let rec datatype P = P1 Q; datatype Q end in pass"
                    , testExpectSuccess "let rec datatype P = P1 Q; datatype Q = Q1 P end in pass"
                    , testExpectSuccess "let rec datatype P = P1 P end in pass"
                    , testExpectSuccess
                          "let rec datatype P = P1 Q; datatype Q = Q1 P; f : P -> P; f p = case p of P1 q -> case q of Q1 p -> p end end end in pass"
                    , testExpectSuccess "let rec datatype P = P1 Q; closedtype Q = Q1 !\"Q1\" end in pass"
                    , testExpectReject "let rec closedtype P = P1 Q; datatype Q = Q1 !\"Q1\" end in pass"
                    , testExpectSuccess
                          "let rec datatype P = P1 Q; datatype Q = Q1 (Action ()); pqpass = P1 (Q1 pass) end in case pqpass of P1 (Q1 p) -> p end"
                    ]
              ]
        , tDecls ["closedtype T = T1 Text Number !\"T.T1\" | T2 !\"T.T2\" | T3 Boolean !\"T.T3\""] $
          tGroup
              "closedtype"
              [ testExpectSuccess "pass"
              , testExpectSuccess "let t1 = T1 \"hello\" 3 in pass"
              , testExpectSuccess "let f (T1 x _) = x in pass"
              , testExpectSuccess "case T1 \"hello\" 3 of T1 \"hello\" 3 -> pass end"
              , testExpectSuccess
                    "case T1 \"hello\" 3 of T2 -> fail \"T2\"; T1 \"hello\" 2 -> fail \"T1 2\"; T1 \"hell\" 3 -> fail \"T1 hell\"; T1 \"hello\" 3 -> pass end"
              , testExpectSuccess "let closedtype P in pass"
              , tGroup
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
              , tGroup
                    "recursive"
                    [ testExpectSuccess "let closedtype P = P1 !\"P1\" in let closedtype Q = Q1 P !\"Q1\" in pass"
                    , testExpectSuccess "let closedtype P = P1 !\"P1\"; closedtype Q = Q1 P !\"Q1\" in pass"
                    , testExpectSuccess "let rec closedtype P = P1 !\"P1\"; closedtype Q = Q1 P !\"Q1\" end in pass"
                    , testExpectSuccess "let rec closedtype P = P1 Q !\"P1\"; closedtype Q end in pass"
                    , testExpectSuccess "let rec closedtype P = P1 Q !\"P1\"; closedtype Q = Q1 P !\"Q1\" end in pass"
                    , testExpectSuccess "let rec closedtype P = P1 P !\"P1\" end in pass"
                    ]
              ]
        , tGroup
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
        , tDecls ["opentype E", "eta = property @E @Text !\"eta\"", "e1 = openEntity @E !\"e1\"", "rt1 = eta !$ {e1}"] $
          tGroup
              "undo"
              [ testExpectSuccess "do rt1 := \"A\"; testeq {\"A\"} rt1; rt1 := \"B\"; testeq {\"B\"} rt1; end"
              , testExpectSuccess
                    "do rt1 := \"A\"; testeq {\"A\"} rt1; rt1 := \"B\"; testeq {\"B\"} rt1; queueUndo; testeq {\"A\"} rt1; end"
              , testExpectSuccess
                    "do rt1 := \"A\"; testeq {\"A\"} rt1; rt1 := \"B\"; testeq {\"B\"} rt1; queueUndo; testeq {\"A\"} rt1; queueRedo; testeq {\"B\"} rt1; end"
              ]
        , tGroup
              "interpret"
              [ testExpectSuccess "do r <- newMemWhole; interpretIntegerAsText r := \"37\"; testeq {37} r; end"
              , testExpectSuccess
                    "do r <- newMemWhole; interpretDateAsText r := \"2015-08-12\"; testeq {YearMonthDay 2015 08 12} r; end"
              ]
        , tDecls
              [ "runresult ar arg = case ar of Left err -> fail err; Right f -> f arg end"
              , "testaction expected action = do found <- action; testeqval expected found end"
              , "testleft action = do found <- action; case found of Left _ -> pass; Right _ -> fail \"not Left\" end end"
              ] $
          tGroup
              "evaluate"
              [ testExpectSuccess "testaction (Right True) $ evaluate @Boolean \"True\""
              , testExpectSuccess "testaction (Right 5) $ evaluate @Integer \"5\""
              , testExpectSuccess "testaction (Right 5) $ evaluate @Integer \"let x = 5 in x\""
              , testExpectSuccess
                    "do ar <- evaluate @(Integer -> Integer) \"\\\\x -> x + 1\"; case ar of Left err -> fail err; Right f -> testeqval 8 $ f 7 end end"
              , testExpectSuccess "testaction (Left \"<evaluate>:1:1: expecting: expression\") $ evaluate @Integer \"\""
              , testExpectSuccess "testaction (Left \"<evaluate>:1:1: undefined: f: a\") $ evaluate @Integer \"f\""
              , testExpectSuccess "testleft $ evaluate @Integer \"\\\"hello\\\"\""
              , testExpectSuccess
                    "do r <- newMemWhole; ar <- evaluate @(WholeRef Integer -> Action ()) \"\\\\r -> r := 45\"; runresult ar r; a <- get r; testeqval 45 a; end"
              ]
        , tGroup
              "text-sort"
              [ testExpectSuccess "testeq {EQ} {alphabetical \"a\" \"a\"}"
              , testExpectSuccess "testeq {EQ} {alphabetical \"A\" \"A\"}"
              , testExpectSuccess "testeq {LT} {alphabetical \"a\" \"A\"}"
              , testExpectSuccess "testeq {LT} {alphabetical \"a\" \"b\"}"
              , testExpectSuccess "testeq {LT} {alphabetical \"A\" \"b\"}"
              , testExpectSuccess "testeq {LT} {alphabetical \"a\" \"B\"}"
              ]
        ]
