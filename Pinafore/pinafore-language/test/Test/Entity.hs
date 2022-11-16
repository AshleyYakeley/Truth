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
    testExpression text text $ \interpret -> do
        action <- interpret
        (sendUpdate, model) <- testerLiftAction action
        testerLiftView $
            runEditor (unWModel $ immutableModelToRejectingModel model) $
            checkUpdateEditor (Known (1 :: Integer)) $ unliftActionOrFail sendUpdate

testUpdates :: TestTree
testUpdates =
    runScriptTestTree $ tGroup "update" [testUpdate "do model <- newMemWholeModel; return (model := 1, model) end"]

data SubtypeResult
    = SRNot
    | SRUnify
    | SRSubsume
    | SRSingle

isOfType :: Text -> Text -> Text
isOfType v t = "let x: " <> t <> " = " <> v <> " in pass"

subtypeTests :: Bool -> SubtypeResult -> Text -> Text -> [ScriptTestTree]
subtypeTests polar sr p q =
    [ testExpectSuccess "pass"
    , tGroup
          "unify"
          [ testSubtypeUnify polar sr $
            "let f: (" <> q <> ") -> Unit = fn _ => (); x: " <> p <> " = undefined; fx = f x in pass"
          , testSubtypeUnify polar sr $
            "let rec f: (" <> q <> ") -> Unit = f end; rec x: " <> p <> " = x; end; fx = f x in pass"
          ]
    , tGroup "subsume" $
      [ testSubtypeSubsume polar sr $ "let rec x: " <> p <> " = x end; y: " <> q <> " = x in pass"
      , testSubtypeSubsume polar sr $ "let x: " <> p <> " = undefined; y: " <> q <> " = x in pass"
      , testSubtypeSubsume polar sr $ "let x: " <> q <> " = undefined: " <> p <> " in pass"
      , testSubtypeSubsume polar sr $ "let x = (undefined: " <> p <> "): " <> q <> " in pass"
      , testSubtypeSingle polar sr $ "let f: (" <> p <> ") -> (" <> q <> ") = fn x => x in pass"
      ]
    ]
  where
    testSubtypeUnify :: Bool -> SubtypeResult -> Text -> ScriptTestTree
    testSubtypeUnify _ SRNot = testExpectReject
    testSubtypeUnify True SRSubsume = testExpectReject
    testSubtypeUnify _ _ = testExpectSuccess
    testSubtypeSubsume :: Bool -> SubtypeResult -> Text -> ScriptTestTree
    testSubtypeSubsume _ SRNot = testExpectReject
    testSubtypeSubsume _ SRUnify = testExpectReject
    testSubtypeSubsume _ SRSubsume = testExpectSuccess
    testSubtypeSubsume _ SRSingle = testExpectSuccess
    testSubtypeSingle :: Bool -> SubtypeResult -> Text -> ScriptTestTree
    testSubtypeSingle False SRSingle = testExpectSuccess
    testSubtypeSingle _ _ = testExpectReject

subtypeTest :: Bool -> SubtypeResult -> Text -> Text -> ScriptTestTree
subtypeTest polar sr p q = tGroup (unpack $ p <> " <: " <> q) $ subtypeTests polar sr p q

strictSubtypeTests :: Text -> Text -> [ScriptTestTree]
strictSubtypeTests p q = [subtypeTest False SRSingle p q, subtypeTest False SRNot q p]

testEntity :: TestTree
testEntity =
    runScriptTestTree $
    tDecls
        [ "pass = return ()"
        , "undefined = error \"undefined\""
        , "runWholeModel = fn r => do a <- get r; a end"
        , "runreforfail = fn r => runWholeModel (r ?? {fail \"unknown model\"})"
        , "testeq = fns expected found => runreforfail {if %expected == %found then pass else fail \"not equal\"}"
        , "testneq = fns expected found => runreforfail {if %expected /= %found then pass else fail \"equal\"}"
        , "testisknown = fn t => runWholeModel {if %(known t) then pass else fail \"known\"}"
        , "testisunknown = fn t => runWholeModel {if %(known t) then fail \"known\" else pass}"
        , "testeqval = fns e f => testeq {e} {f}"
        , "expectStop = fn p => onStop (p >> fail \"no stop\") pass"
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
        , tDecls ["flagRef = do r <- newMemWholeModel; r := False; return r; end"] $
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
              "model"
              [ tGroup
                    "notation"
                    [ testExpectSuccess "runreforfail {pass}"
                    , testExpectSuccess "let p = pass in runreforfail {p}"
                    , testExpectSuccess "runreforfail {let p = pass in p}"
                    , testExpectSuccess "runreforfail {%{pass}}"
                    , testExpectSuccess "let rp = {pass} in runreforfail {%rp}"
                    , testExpectSuccess "runreforfail {let rp = {pass} in %rp}"
                    , testExpectSuccess "let rp = {pass} in runreforfail {let p= %rp in p}"
                    ]
              , tGroup
                    "stop"
                    [ testExpectSuccess "expectStop $ stop"
                    , testExpectSuccess "expectStop $ get unknown"
                    , testExpectSuccess "expectStop $ {1} := 1"
                    , testExpectSuccess "expectStop $ delete {1}"
                    ]
              , tGroup
                    "memory"
                    [ testExpectSuccess "expectStop $ do r <- newMemWholeModel; get r; end"
                    , testExpectSuccess "do r <- newMemWholeModel; r := 45; a <- get r; testeqval 45 a; end"
                    , testExpectSuccess "do r <- newMemWholeModel; r := 3; r := 4; a <- get r; testeqval 4 a; end"
                    , testExpectSuccess
                          "do s <- newMemFiniteSetModel; n <- get $ finiteSetModelCount s; testeqval 0 n; end"
                    , testExpectSuccess
                          "do s <- newMemFiniteSetModel; s += 57; n <- get $ finiteSetModelCount s; testeqval 1 n; end"
                    , testExpectSuccess
                          "do s <- newMemFiniteSetModel; s -= 57; n <- get $ finiteSetModelCount s; testeqval 0 n; end"
                    , testExpectSuccess
                          "do s <- newMemFiniteSetModel; s += 57; s -= 57; n <- get $ finiteSetModelCount s; testeqval 0 n; end"
                    , testExpectSuccess
                          "do s <- newMemFiniteSetModel; s += 57; m54 <- get $ member s {54}; m57 <- get $ member s {57}; testeqval False m54; testeqval True m57; end"
                    , testExpectSuccess
                          "do s <- newMemFiniteSetModel; s -= 57; m57 <- get $ member s {57}; testeqval False m57; end"
                    , testExpectSuccess
                          "do s <- newMemFiniteSetModel; s += 57; s -= 57; m57 <- get $ member s {57}; testeqval False m57; end"
                    , testExpectSuccess
                          "do s <- newMemFiniteSetModel; member s {57} := True; m54 <- get $ member s {54}; m57 <- get $ member s {57}; testeqval False m54; testeqval True m57; end"
                    , testExpectSuccess
                          "do s <- newMemFiniteSetModel; member s {57} := False; m57 <- get $ member s {57}; testeqval False m57; end"
                    , testExpectSuccess
                          "do s <- newMemFiniteSetModel; member s {57} := True; member s {57} := False; m57 <- get $ member s {57}; testeqval False m57; end"
                    , testExpectSuccess "expectStop $ do r <- newMemWholeModel; immutWholeModel r := 5; end"
                    ]
              , tDecls
                    [ "showVal: Showable -> Action Unit = fn v => Debug.message $ show v"
                    , "showList: List Showable -> Action Unit = fn l => do Debug.message \"[[[\"; for_ l showVal;  Debug.message \"]]]\"; end"
                    , "testImmutList = fns present n call => do lr <- newMemListModel; lr := [10,20,30]; r <- listModelItem present n lr; ir <- listModelItem present n $ immutListModel lr; call lr; a <- get r; ia <- get ir; testeqval a ia; end"
                    ] $
                tGroup
                    "list"
                    [ testExpectSuccess "pass"
                    , testExpectSuccess "do r <- newMemListModel; n <- listModelGetCount r; testeqval 0 n; end"
                    , testExpectSuccess
                          "do r <- newMemListModel; r := [10,20,30]; n <- listModelGetCount r; testeqval 3 n; end"
                    , testExpectSuccess "do r <- newMemListModel; n <- get $ listModelCount r; testeqval 0 n; end"
                    , testExpectSuccess
                          "do r <- newMemListModel; r := [10,20,30]; n <- get $ listModelCount r; testeqval 3 n; end"
                    , testExpectSuccess
                          "do r <- newMemListModel; r := [10,20,30]; ir <- listModelItem True 1 r; i <- get ir; testeqval 20 i; end"
                    , testExpectSuccess
                          "do r <- newMemListModel; r := [10,20,30]; ir <- listModelItem True 1 r; ir := 25; i <- get ir; testeqval 25 i; end"
                    , testExpectSuccess
                          "do r <- newMemListModel; r := [10,20,30]; ir <- listModelItem True 1 r; ir := 25; l <- get r; testeqval [10,25,30] l; end"
                    , testExpectSuccess
                          "do r <- newMemListModel; r := [10,20,30]; ir <- listModelItem True 1 r; delete ir; l <- get r; testeqval [10,30] l; end"
                    , testExpectSuccess
                          "do r <- newMemListModel; r := [10,20,30]; ir <- listModelItem True 1 r; delete ir; ir := 15; l <- get r; testeqval [10,15,30] l; end"
                    , testExpectSuccess
                          "do r <- newMemListModel; r := [10,20,30]; ir <- listModelItem False 1 r; i <- expectStop $ get ir; return (); end"
                    , testExpectSuccess
                          "do r <- newMemListModel; r := [10,20,30]; ir <- listModelItem False 1 r; ir := 25; i <- get ir; testeqval 25 i; end"
                    , testExpectSuccess
                          "do r <- newMemListModel; r := [10,20,30]; ir <- listModelItem False 1 r; ir := 25; l <- get r; testeqval [10,25,20,30] l; end"
                    , testExpectSuccess
                          "do r <- newMemListModel; r := [10,20,30]; ir <- listModelItem False 1 r; delete ir; l <- get r; testeqval [10,20,30] l; end"
                    , testExpectSuccess
                          "do r <- newMemListModel; r := [10,20,30]; ir <- listModelItem False 1 r; delete ir; ir := 15; l <- get r; testeqval [10,15,20,30] l; end"
                    , testExpectSuccess
                          "do r <- newMemListModel; r := [10,20,30]; ir <- listModelItem False 1 r; delete ir; l <- get r; testeqval [10,20,30] l; end"
                    , testExpectSuccess
                          "do r <- newMemListModel; r := [10,20,30]; ir <- listModelItem False 1 r; delete ir; ir := 15; l <- get r; testeqval [10,15,20,30] l; end"
                    , testExpectSuccess
                          "do r <- newMemListModel; r := [10,20,30]; ir <- listModelItem True 1 r; listModelInsert 1 12 r; i <- get ir; testeqval 20 i; end"
                    , testExpectSuccess
                          "do r <- newMemListModel; r := [10,20,30]; ir <- listModelItem True 1 r; listModelInsert 1 12 r; ir := 15; l <- get r; testeqval [10,12,15,30] l; end"
                    , testExpectSuccess "testImmutList True 1 $ fn _ => return ()"
                    ]
              ]
        , tDecls
              [ "convr : Rational -> Rational= id"
              , "convn : Number -> Number = id"
              , "convl : Literal -> Literal = id"
              , "testconvr : Rational -> Action Unit = fn r => testeq {convl r} {convl $ convn r}"
              ] $
          tGroup
              "literal"
              [ tGroup
                    "Rational to Number"
                    [ testExpectSuccess "testconvr 1"
                    , testExpectSuccess "testconvr 2.5"
                    , testExpectSuccess "testeq {convl 31.5} {convl $ convn 31.5}"
                    , testExpectSuccess "testeq {\"63/2\"} {show 31.5}"
                    , testExpectSuccess "testeq {\"63/2\"} {show $ convn 31.5}"
                    ]
              , let
                    testLiteralConversion :: ScriptExpectation -> Text -> Text -> ScriptTestTree
                    testLiteralConversion se ptype val =
                        testScriptExpectation (val <> ": " <> ptype <> " => " <> pack (show se)) se $
                        "((" <>
                        val <>
                        "): Literal) >- match val:? " <> ptype <> " => testeqval val (" <> val <> "); _ => stop end"
                    testPairs :: [(Text, Text)]
                    testPairs =
                        [ ("Unit", "()")
                        , ("Boolean", "True")
                        , ("Ordering", "LT")
                        , ("Integer", "0")
                        , ("Integer", "1")
                        , ("Integer", "-4")
                        , ("Rational", "2.5")
                        , ("Number", "~31.5")
                        , ("Text", "\"Hello\"")
                        , ("Text", "\"2.5\"")
                        , ("Text", "\"()\"")
                        , ("Duration", "Seconds 3600")
                        , ( "Time"
                          , "localToTime -480 $ DateAndTime (YearMonthDay 2022 01 16) (HourMinuteSecond 19 07 22)")
                        , ("Date", "YearMonthDay 2022 01 16")
                        , ("TimeOfDay", "HourMinuteSecond 19 07 22")
                        , ("LocalTime", "DateAndTime (YearMonthDay 2022 01 16) (HourMinuteSecond 19 07 22)")
                        ]
                    testTypes :: [Text]
                    testTypes = "Literal" : nub (fmap fst testPairs)
                    expectation :: Text -> Text -> ScriptExpectation
                    expectation "Integer" "Number" = ScriptExpectSuccess
                    expectation "Integer" "Rational" = ScriptExpectSuccess
                    expectation "Rational" "Number" = ScriptExpectSuccess
                    expectation _ "Literal" = ScriptExpectSuccess
                    expectation valtype casttype
                        | valtype == casttype = ScriptExpectSuccess
                    expectation _ _ = ScriptExpectStop
                    in tGroup
                           "general"
                           [ testLiteralConversion (expectation valtype casttype) casttype val
                           | casttype <- testTypes
                           , (valtype, val) <- testPairs
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
              "storage"
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
                    , testExpectSuccess "tea !$ {\"hello\"} := e1 >> testeq {1} (finiteSetModelCount (tea !@ {e1}))"
                    , testExpectSuccess "(eea !. eea) !$ {e1} := e2"
                    , testExpectSuccess
                          "do (eea !. eea) !$ {e1} := e2; testeq {e2} ((eea !. eea) !$ {e1}); testeq {e2} (eea !$ (eea !$ {e1})); end"
                    , testExpectSuccess
                          "do eea !$ (eea !$ {e1}) := e2; testeq {e2} ((eea !. eea) !$ {e1}); testeq {e2} (eea !$ (eea !$ {e1})); end"
                    , testExpectSuccess "expectStop $ do r <- newMemWholeModel; eia !$ r := 4; end"
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
                    "finiteSetModelClear"
                    [ testExpectSuccess
                          "eta !@ {\"hello\"} += e1 >> finiteSetModelClear (eta !@ {\"hello\"}) >> testisunknown (eta !$ {e1})"
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
                          "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> finiteSetModelClear ((eta !. eeb) !@ {\"hello\"}) >> testeq {e2} (eeb !$ {e1})"
                    , testExpectSuccess
                          "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> finiteSetModelClear ((eta !. eeb) !@ {\"hello\"}) >> testisunknown (eta !$ {e2})"
                    , testExpectSuccess
                          "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> finiteSetModelClear (eeb !@@ eta !@ {\"hello\"}) >> testneq {e2} (eeb !$ {e1})"
                    , testExpectSuccess
                          "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> finiteSetModelClear (eeb !@@ eta !@ {\"hello\"}) >> testeq {\"hello\"} (eta !$ {e2})"
                    ]
              , tGroup
                    "finiteSetModelSingle"
                    [ testExpectSuccess "testisunknown (finiteSetModelSingle $ eib !$$ eia !@ {0})"
                    , testExpectSuccess
                          "eib !$ {e1} := 1 >> eia !$ {e1} := 0 >> testeq {1} (finiteSetModelSingle $ eib !$$ eia !@ {0})"
                    , testExpectSuccess
                          "eib !$ {e1} := 1 >> eia !$ {e1} := 0 >> eic !$ {e1} := 0 >> testeq {1} (finiteSetModelSingle $ eib !$$ eia !@ {0})"
                    , testExpectSuccess
                          "eib !$ {e1} := 1 >> eia !$ {e1} := 0 >> eia !$ {e1} := 0 >> testeq {1} (finiteSetModelSingle $ eib !$$ eia !@ {0})"
                    , testExpectSuccess
                          "eib !$ {e1} := 1 >> eib !$ {e2} := 2 >> eia !$ {e1} := 0 >> eia !$ {e2} := 0 >> testisunknown (finiteSetModelSingle $ eib !$$ eia !@ {0})"
                    , testExpectSuccess
                          "eib !$ {e1} := 1 >> eib !$ {e2} := 1 >> eia !$ {e1} := 0 >> eia !$ {e2} := 0 >> testeq {1} (finiteSetModelSingle $ eib !$$ eia !@ {0})"
                    ]
              , tGroup
                    "multiple set member"
                    [ testExpectSuccess "testeq {0} (finiteSetModelCount (tea !@ {e1}))"
                    , testExpectSuccess "eea !$ {e2} := e1 >> testeq {1} (finiteSetModelCount (eea !@ {e1}))"
                    , testExpectSuccess "eea !@ {e1} += e2 >> testeq {1} (finiteSetModelCount (eea !@ {e1}))"
                    , testExpectSuccess "tea !$ {\"hello\"} := e1 >> testeq {e1} (tea !$ {\"hello\"})"
                    , testExpectSuccess "tea !@ {e1} += \"hello\" >> testeq {e1} (tea !$ {\"hello\"})"
                    , testExpectSuccess "tea !$ {\"hello\"} := e1 >> testeq {1} (finiteSetModelCount (tea !@ {e1}))"
                    , testExpectSuccess "tea !@ {e1} += \"hello\" >> testeq {1} (finiteSetModelCount (tea !@ {e1}))"
                    , testExpectSuccess
                          "tea !@ {e1} += \"hello\" >> tea !@ {e1} += \"hello\" >> testeq {1} (finiteSetModelCount (tea !@ {e1}))"
                    , testExpectSuccess
                          "tea !@ {e1} += \"h\" >> tea !@ {e1} += \"hello\" >> testeq {2} (finiteSetModelCount (tea !@ {e1}))"
                    , testExpectSuccess $
                      "let counter = eia !$ {e1};someset = nea !@ {e1} in " <>
                      "counter := 0 >> someset += 1 >> someset += 1 >> (get (finiteSetModelList noOrder someset) >>= fn pp => for pp $ fn p => runWholeModel {counter := %counter + 1}) >> testeq {1} counter"
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
                          "let enta = property @E @(List Text) !\"enta\" in enta !$ {e1} := [\"abc\", \"def\"] >> (testeq {[\"abc\", \"def\"]} $ enta !$ {e1})"
                    , testExpectSuccess
                          "let enta = property @E @(List Text) !\"enta\" in enta !$ {e1} := [] >> (testeq {[]} $ enta !$ {e1})"
                    ]
              , tGroup
                    "Pair/Either"
                    [ testExpectSuccess
                          "let enta = property @E @(Number *: Text) !\"enta\" in enta !$ {e1} := (74,\"hmm\") >> (testneq {(71,\"hmm\")} $ enta !$ {e1})"
                    , testExpectSuccess
                          "let enta = property @E @(Number *: Text) !\"enta\" in enta !$ {e1} := (74,\"hmm\") >> (testeq {(74,\"hmm\")} $ enta !$ {e1})"
                    , testExpectSuccess
                          "let enta = property @E @(Number +: Text) !\"enta\" in enta !$ {e1} := Left 74 >> (testneq {Left 73} $ enta !$ {e1})"
                    , testExpectSuccess
                          "let enta = property @E @(Number +: Text) !\"enta\" in enta !$ {e1} := Left 74 >> (testeq {Left 74} $ enta !$ {e1})"
                    , testExpectSuccess
                          "let enta = property @E @(Number +: Text) !\"enta\" in enta !$ {e1} := Right \"abc\" >> (testneq {Right \"adbc\"} $ enta !$ {e1})"
                    , testExpectSuccess
                          "let enta = property @E @(Number +: Text) !\"enta\" in enta !$ {e1} := Right \"abc\" >> (testeq {Right \"abc\"} $ enta !$ {e1})"
                    ]
              ]
        , tGroup
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
                    , subtypeTest False SRSubsume "List a" "List Integer"
                    , subtypeTest False SRUnify "List Integer" "List a"
                    , subtypeTest False SRSubsume "a *: b" "Integer *: Rational"
                    , subtypeTest False SRSubsume "a *: a" "Integer *: Integer"
                    , subtypeTest False SRSubsume "a *: a" "Integer *: Rational"
                    , subtypeTest False SRSubsume "a *: Text" "Integer *: Text"
                    , subtypeTest False SRSingle "List a" "List a"
                    , tGroup
                          "ModelOrder"
                          [ subtypeTest False SRSingle "a -> a -> Ordering" "ModelOrder a"
                          , subtypeTest False SRSingle "Integer -> Integer -> Ordering" "ModelOrder Integer"
                          ]
                    , tGroup
                          "models"
                          [ subtypeTest False SRSingle "ListModel a" "WholeModel (List a)"
                          , subtypeTest False SRSingle "ListModel Integer" "WholeModel (List Integer)"
                          , subtypeTest False SRSingle "ListModel Integer" "WholeModel {-List Integer,+List Integer}"
                          , subtypeTest
                                True
                                SRSingle
                                "ListModel {-(a & Integer),+(a | Integer)}"
                                "WholeModel (List Integer)"
                          , subtypeTest
                                True
                                SRSingle
                                "ListModel {-(a & Entity),+(a | Integer)}"
                                "WholeModel (List Integer)"
                          , tGroup
                                "Model"
                                [ subtypeTest False SRSingle "WholeModel {-Integer,+Text}" "Model"
                                , subtypeTest False SRSingle "ListModel {-Text,+Text}" "Model"
                                , subtypeTest False SRSingle "ListModel {-Integer,+Text}" "Model"
                                , subtypeTest False SRSingle "TextModel " "Model"
                                , subtypeTest False SRSingle "SetModel Text" "Model"
                                , subtypeTest False SRSingle "FiniteSetModel {-Integer,+Text}" "Model"
                                ]
                          ]
                    , tGroup
                          "recursive"
                          [ subtypeTest False SRSingle "rec a. Maybe a" "rec a. Maybe a"
                          , subtypeTest False SRSingle "rec a. Maybe a" "rec b. Maybe b"
                          , subtypeTest False SRSingle "rec a. Maybe a" "Maybe (rec a. Maybe a)"
                          , subtypeTest False SRSingle "rec a. Maybe a" "Maybe (rec b. Maybe b)"
                          , subtypeTest False SRSingle "Maybe (rec a. Maybe a)" "rec a. Maybe a"
                          , subtypeTest False SRSingle "Maybe (rec a. Maybe a)" "rec b. Maybe b"
                          , subtypeTest False SRSingle "Maybe (rec a. Maybe a)" "Maybe (rec a. Maybe a)"
                          , subtypeTest False SRSingle "Maybe (rec a. Maybe a)" "Maybe (rec b. Maybe b)"
                          ]
                    ]
              , tGroup
                    "let"
                    [ tDecls ["opentype P", "opentype Q", "subtype P <: Q"] $ tGroup "seq" $ strictSubtypeTests "P" "Q"
                    , tDeclsRec ["opentype P", "opentype Q", "subtype P <: Q"] $
                      tGroup "rec 1" $ strictSubtypeTests "P" "Q"
                    , tDeclsRec ["opentype P", "subtype P <: Q", "opentype Q"] $
                      tGroup "rec 2" $ strictSubtypeTests "P" "Q"
                    , tDeclsRec ["subtype P <: Q", "opentype P", "opentype Q"] $
                      tGroup "rec 3" $ strictSubtypeTests "P" "Q"
                    ]
              , tGroup
                    "local"
                    [ tDecls ["opentype P"] $
                      tGroup
                          "1"
                          [ testExpectSuccess "pass"
                          , testExpectSuccess "let opentype Q; subtype P <: Q in pass"
                          , testExpectSuccess "let opentype Q; subtype P <: Q; f : P -> Q = fn x => x in pass"
                          , testExpectReject "let opentype Q; subtype P <: Q; f : Q -> P = fn x => x in pass"
                          ]
                    , tDecls ["opentype Q"] $
                      tGroup
                          "2"
                          [ testExpectSuccess "pass"
                          , testExpectSuccess "let opentype P; subtype P <: Q in pass"
                          , testExpectSuccess "let opentype P; subtype P <: Q; f : P -> Q = fn x => x in pass"
                          , testExpectReject "let opentype P; subtype P <: Q; f : Q -> P = fn x => x in pass"
                          ]
                    , tDecls ["opentype P", "opentype Q"] $
                      tGroup
                          "3"
                          [ testExpectSuccess "pass"
                          , testExpectSuccess "let subtype P <: Q in pass"
                          , testExpectSuccess "let subtype P <: Q; f : P -> Q = fn x => x in pass"
                          , testExpectReject "let subtype P <: Q; f : Q -> P = fn x => x in pass"
                          ]
                    ]
              , tGroup
                    "circular"
                    [ tDecls ["opentype P", "subtype P <: P"] $
                      tGroup
                          "singular"
                          [ testExpectSuccess "pass"
                          , testExpectSuccess "let f : P -> P = fn x => x in pass"
                          , testExpectSuccess "let f : List P -> List P = fn x => x in pass"
                          ]
                    , tDecls ["opentype P", "opentype Q", "subtype P <: Q", "subtype Q <: P"] $
                      tGroup
                          "pair"
                          [ testExpectSuccess "pass"
                          , testExpectSuccess "let f : P -> P = fn x => x in pass"
                          , testExpectSuccess "let f : Q -> Q = fn x => x in pass"
                          , testExpectSuccess "let f : P -> Q = fn x => x in pass"
                          , testExpectSuccess "let f : List P -> List Q = fn x => x in pass"
                          , testExpectSuccess "let f : Q -> P = fn x => x in pass"
                          ]
                    ]
              , tDecls ["opentype Q", "subtype Maybe Number <: Q"] $
                tGroup
                    "non-simple" -- not allowed, per issue #28
                    [testExpectReject "pass"]
              , tDecls ["opentype Q", "subtype Integer <: Q"] $ tGroup "literal" $ strictSubtypeTests "Integer" "Q"
              , tDecls ["opentype Q", "closedtype P of P1 Text Number !\"P.P1\" end", "subtype P <: Q"] $
                tGroup "closed" $ strictSubtypeTests "P" "Q"
              , tDecls
                    [ "opentype Q"
                    , "opentype R"
                    , "closedtype P of P1 Text Number !\"P.P1\" end"
                    , "subtype P <: Q"
                    , "subtype P <: R"
                    ] $
                tGroup "closed" $ strictSubtypeTests "P" "R"
              , tGroup
                    "Entity"
                    [ testExpectSuccess "let f : Number -> Entity = fn x => x in pass"
                    , testExpectSuccess "let f : (a & Number) -> Entity *: a = fn x => (x,x) in pass"
                    , testExpectSuccess "let f : Maybe Number -> Entity = fn x => x in pass"
                    , testExpectSuccess "let f : Maybe (a & Number) -> Entity *: Maybe a = fn x => (x,x) in pass"
                    ]
              , tGroup "dynamic" $
                [ tGroup "DynamicEntity <: Entity" $ strictSubtypeTests "DynamicEntity" "Entity"
                , tDecls ["dynamictype P1 = !\"P1\"", "dynamictype P2 = !\"P2\"", "dynamictype Q = P1 | P2"] $
                  tGroup "P1 <: DynamicEntity" $ strictSubtypeTests "P1" "DynamicEntity"
                , tDecls ["dynamictype P1 = !\"P1\"", "dynamictype P2 = !\"P2\"", "dynamictype Q = P1 | P2"] $
                  tGroup "Q <: DynamicEntity" $ strictSubtypeTests "Q" "DynamicEntity"
                , tDecls ["dynamictype P1 = !\"P1\"", "dynamictype P2 = !\"P2\"", "dynamictype Q = P1 | P2"] $
                  tGroup "P1 <: Entity" $ strictSubtypeTests "P1" "Entity"
                , tDecls ["dynamictype P1 = !\"P1\"", "dynamictype P2 = !\"P2\"", "dynamictype Q = P1 | P2"] $
                  tGroup "Q <: Entity" $ strictSubtypeTests "Q" "Entity"
                , tDecls ["dynamictype P1 = !\"P1\"", "dynamictype P2 = !\"P2\"", "dynamictype Q = P1 | P2"] $
                  tGroup "seq" $ strictSubtypeTests "P1" "Q"
                , tGroup "recursive" $ let
                      pqtests =
                          [subtypeTest False SRNot "P1" "P2", subtypeTest False SRNot "P2" "P1"] <>
                          strictSubtypeTests "P1" "Q" <> strictSubtypeTests "P2" "Q"
                      in [ tDeclsRec ["dynamictype P1 = !\"P1\""] $ subtypeTest False SRSingle "P1" "P1"
                         , tDeclsRec ["dynamictype P1 = !\"P1\"", "dynamictype P2 = !\"P2\"", "dynamictype Q = P1 | P2"] $
                           tGroup "rec 1" pqtests
                         , tDeclsRec ["dynamictype P1 = !\"P1\"", "dynamictype Q = P1 | P2", "dynamictype P2 = !\"P2\""] $
                           tGroup "rec 2" pqtests
                         , tDeclsRec ["dynamictype Q = P1 | P2", "dynamictype P1 = !\"P1\"", "dynamictype P2 = !\"P2\""] $
                           tGroup "rec 3" pqtests
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
                               [ tGroup "QC <: QB" $ strictSubtypeTests "QC" "QB"
                               , tGroup "QA = QB" $ subtypeTests False SRSingle "QA" "QB"
                               , tGroup "QA = QB" $ subtypeTests False SRSingle "QB" "QA"
                               , tGroup "QA <: T" $ strictSubtypeTests "QA" "T"
                               , tGroup "QB <: T" $ strictSubtypeTests "QB" "T"
                               , tGroup "QC <: T" $ strictSubtypeTests "QC" "T"
                               , tGroup "P1 <: T" $ strictSubtypeTests "P1" "T"
                               ]
                         , tGroup
                               "cycle"
                               [ tDeclsRec ["dynamictype P = P"] $ testExpectReject "pass"
                               , tDeclsRec ["dynamictype P = Q", "dynamictype Q = P"] $ testExpectReject "pass"
                               , tDeclsRec ["dynamictype P = Q", "dynamictype Q = P"] $
                                 testExpectReject "let f: P -> Q; f x = x in pass"
                               , tDeclsRec ["dynamictype P1 = !\"P1\"", "dynamictype P = P1 | Q", "dynamictype Q = P"] $
                                 testExpectReject "pass"
                               , tDeclsRec
                                     ["dynamictype P1 = !\"P1\"", "dynamictype P = P1 | Q", "dynamictype Q = P | Q"] $
                                 testExpectReject "pass"
                               , tDeclsRec ["dynamictype P1 = !\"P1\"", "dynamictype Q = P1 | Q"] $
                                 testExpectReject "pass"
                               ]
                         ]
                ]
              ]
        , tGroup
              "greatest-dynamic-supertype"
              [ tGroup
                    "Literal"
                    [ testExpectSuccess "testeqval 1 $ 34.0 >- match 34 => 1; True => 2; \"hello\" => 3; _ => 4 end"
                    , testExpectSuccess "testeqval 2 $ True >- match 34 => 1; True => 2; \"hello\" => 3; _ => 4 end"
                    , testExpectSuccess
                          "testeqval 3 $ \"hello\" >- match 34 => 1; True => 2; \"hello\" => 3; _ => 4 end"
                    , testExpectSuccess "testeqval 4 $ () >- match 34 => 1; True => 2; \"hello\" => 3; _ => 4 end"
                    , testExpectSuccess
                          "testeqval 1 $ 34.0 >- match _:?Integer => 1; _:?Boolean => 2; _:?Text => 3; _ => 4 end"
                    , testExpectSuccess
                          "testeqval 2 $ True >- match _:?Integer => 1; _:?Boolean => 2; _:?Text => 3; _ => 4 end"
                    , testExpectSuccess
                          "testeqval 3 $ \"hello\" >- match _:?Integer => 1; _:?Boolean => 2; _:?Text => 3; _ => 4 end"
                    , testExpectSuccess
                          "testeqval 4 $ () >- match _:?Integer => 1; _:?Boolean => 2; _:?Text => 3; _ => 4 end"
                    , testExpectSuccess
                          "testeqval 1 $ 34.0 >- match _:?Integer => 1; _:?Rational => 2; _:?Text => 3; _ => 4 end"
                    , testExpectSuccess
                          "testeqval 2 $ 34.0 >- match _:?Rational => 2; _:?Integer => 1; _:?Text => 3; _ => 4 end"
                    ]
              , tGroup
                    "List"
                    [ testExpectSuccess "testeqval 2 $ [] >- match _ :: _ => 1; [] => 2 end"
                    , testExpectSuccess "testeqval 2 $ [] >- match _ :: _ => 1; _ => 2 end"
                    , testExpectSuccess "testeqval 2 $ [] >- match _:? List1 Integer => 1; _ => 2 end"
                    , testExpectSuccess "testeqval 1 $ [3,4] >- match _ :: _ => 1; [] => 2 end"
                    , testExpectSuccess "testeqval 1 $ [3,4] >- match _ :: _ => 1; _ => 2 end"
                    , testExpectSuccess "testeqval 1 $ [3,4] >- match _:? List1 Integer => 1; _ => 2 end"
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
              , testExpectSuccess "testeq {True} {e1 >- match _:? P1 => True; _ => False end}"
              , testExpectSuccess "testeq {False} {e1 >- match _:? P2 => True; _ => False end}"
              , testExpectSuccess "testeq {True} {e1 >- match _:? Q => True; _ => False end}"
              , testExpectSuccess "testeq {e1} {coerce @P1 e1}"
              , testExpectSuccess "testeq {e1} {coerce @Q e1}"
              ]
        , tDecls
              [ "datatype T of T1 Text Number; T2; T3 Boolean; T4 (WholeModel {-Boolean,+Integer} -> Integer); T5 Text (Boolean -> Integer) end"
              ] $
          tGroup
              "datatype"
              [ testExpectSuccess "pass"
              , testExpectSuccess "let t1 = T1 \"hello\" 3 in pass"
              , testExpectSuccess "let f = fn T1 x _ => x in pass"
              , testExpectSuccess "T2 >- match T2 => pass end"
              , testExpectSuccess "T3 True >- match T3 True => pass end"
              , testExpectSuccess "T1 \"hello\" 3 >- match T1 \"hello\" 3 => pass end"
              , testExpectSuccess
                    "T1 \"hello\" 3 >- match T2 => fail \"T2\"; T1 \"hello\" 2 => fail \"T1 2\"; T1 \"hell\" 3 => fail \"T1 hell\"; T1 \"hello\" 3 => pass end"
              , testExpectSuccess
                    "let f : Boolean -> Integer = fn b => if b then 1 else 0 in T5 \"abcd\" f >- match T5 _ ff => if ff True == 1 then pass else fail \"ff\" end"
              , testExpectReject "let datatype B of MkB a end in pass"
              , testExpectSuccess "let datatype P of end in pass"
              , testExpectSuccess "let datatype P of P1 end in pass"
              , testExpectSuccess "let datatype P of P1; end in pass"
              , testExpectSuccess "let datatype P of P1 Integer end in pass"
              , testExpectSuccess "let datatype P of P1 Integer; end in pass"
              , testExpectSuccess "let datatype P of P1 Integer; P2 Text end in pass"
              , testExpectSuccess "let datatype P of P1 Integer; P2 Text; end in pass"
              , tGroup
                    "nominal"
                    [ testExpectSuccess "let datatype P of P1 end; f : P -> P = fn x => x in pass"
                    , testExpectReject "let datatype P of P1 end; datatype Q of end; f : P -> Q = fn x => x in pass"
                    , testExpectReject "let datatype P of end; datatype Q of Q1 end; f : P -> Q = fn x => x in pass"
                    , testExpectReject "let datatype P of end; datatype Q of end; f : P -> Q = fn x => x in pass"
                    , testExpectReject "let datatype P of P1 end; datatype Q of Q1 end; f : P -> Q = fn x => x in pass"
                    , testExpectReject
                          "let datatype P of P1 Integer end; datatype Q of Q1 Integer end; f : P -> Q = fn x => x in pass"
                    ]
              , tGroup
                    "recursive"
                    [ testExpectSuccess "let datatype P of P1 end in let datatype Q of Q1 P end in pass"
                    , testExpectSuccess "let datatype P of P1 end; datatype Q of Q1 P end in pass"
                    , testExpectSuccess "let rec datatype P of P1 Q end; datatype Q of end end in pass"
                    , testExpectSuccess "let rec datatype P of P1 Q end; datatype Q of Q1 P end end in pass"
                    , testExpectSuccess "let rec datatype P of P1 P end end in pass"
                    , testExpectSuccess
                          "let rec datatype P of P1 Q end; datatype Q of Q1 P end; f : P -> P = match P1 q => q >- match Q1 p => p end end end in pass"
                    , testExpectSuccess "let rec datatype P of P1 Q end; closedtype Q of Q1 !\"Q1\" end end in pass"
                    , testExpectReject "let rec closedtype P of P1 Q end; datatype Q of Q1 !\"Q1\" end end in pass"
                    , testExpectSuccess
                          "let rec datatype P of P1 Q end; datatype Q of Q1 (Action Unit) end; pqpass = P1 (Q1 pass) end in pqpass >- match P1 (Q1 p) => p end"
                    ]
              , tGroup
                    "parameters"
                    [ tGroup
                          "variance"
                          [ testExpectSuccess "let datatype B +a of MkB a end in pass"
                          , testExpectReject "let datatype B -a of MkB a end in pass"
                          , testExpectSuccess "let datatype B -a of MkB (a -> Boolean) end in pass"
                          , testExpectReject "let datatype B +a of MkB (a -> Boolean) end in pass"
                          , testExpectSuccess "let datatype B {-p,+q} of MkB (p -> q) end in pass"
                          , testExpectSuccess "let datatype B {+q,-p} of MkB (p -> q) end in pass"
                          , testExpectReject "let datatype B {-p,+q} of MkB (q -> p) end in pass"
                          , testExpectReject "let datatype B {+q,-p} of MkB (q -> p) end in pass"
                          ]
                    , tGroup
                          "recursive"
                          [ testExpectSuccess "let rec datatype R +a of MkR (R a) end end in pass"
                          , testExpectSuccess "let rec datatype R -a of MkR (R a) end end in pass"
                          , testExpectSuccess
                                "let rec datatype R1 +a of MkR1 (R2 a) end; datatype R2 +a of MkR2 (R1 a) end end in pass"
                          , testExpectSuccess
                                "let rec datatype R1 -a of MkR1 (R2 a) end; datatype R2 -a of MkR2 (R1 a) end end in pass"
                          , testExpectSuccess
                                "let rec datatype R1 +a of MkR1 (R2 a -> Integer) end; datatype R2 -a of MkR2 (R1 a -> Integer) end end in pass"
                          ]
                    , tGroup
                          "conversion"
                          [ tDecls
                                [ "datatype D +a of Mk1D (List a); Mk2D (Maybe a) end"
                                , "showD: D Showable -> Text = match Mk1D aa => show aa; Mk2D ma => show ma end"
                                , "di: D Integer = Mk1D [576,469,12]"
                                , "sdi: Text = showD di"
                                ] $
                            testExpectSuccess "if sdi == \"[576, 469, 12]\" then pass else fail sdi"
                          , tDecls
                                [ "datatype D -a of Mk1D (a -> Integer); Mk2D (a -> a -> Text) end"
                                , "dShow: D Number = Mk2D $ fns a b => show a <> \",\" <> show b"
                                , "di: D Integer = dShow"
                                , "showD: a -> D a -> Text = fn a => match Mk1D ai => show $ ai a; Mk2D aat => aat a a end"
                                , "sd: Text = showD 356 di"
                                ] $
                            testExpectSuccess "if sd == \"356,356\" then pass else fail sd"
                          , tDecls
                                [ "rec datatype RList +a of MkRList (Maybe (a *: RList a)) end end"
                                , "rec showRList: RList Showable -> Text = fn MkRList rl => rl >- match Nothing => \"\"; Just (a,rla) => show a <> \";\" <> showRList rla end end"
                                , "rlisti: RList Integer = MkRList $ Just (45,MkRList $ Just (72, MkRList $ Just (18,MkRList Nothing)))"
                                , "rlists: RList Showable = rlisti"
                                , "sd: Text = showRList rlists"
                                ] $
                            testExpectSuccess "if sd == \"45;72;18;\" then pass else fail sd"
                          ]
                    ]
              , tGroup
                    "subtype"
                    [ tDecls
                          [ "datatype D1 of C1 Integer; subtype datatype D2 of C2; subtype datatype D3 of C3 Boolean; end; end; end"
                          ] $
                      tGroup
                          "T"
                          [ testExpectSuccess "pass"
                          , testExpectSuccess $ isOfType "C1 3" "D1"
                          , testExpectReject $ isOfType "C1 3" "D2"
                          , testExpectReject $ isOfType "C1 3" "D3"
                          , testExpectSuccess $ isOfType "C2" "D1"
                          , testExpectSuccess $ isOfType "C2" "D2"
                          , testExpectReject $ isOfType "C2" "D3"
                          , testExpectSuccess $ isOfType "C3 True" "D1"
                          , testExpectSuccess $ isOfType "C3 True" "D2"
                          , testExpectSuccess $ isOfType "C3 True" "D3"
                          , subtypeTest False SRSingle "D2" "D1"
                          , subtypeTest False SRSingle "D3" "D2"
                          , subtypeTest False SRSingle "D3" "D1"
                          , subtypeTest False SRNot "D1" "D2"
                          , subtypeTest False SRNot "D2" "D3"
                          , subtypeTest False SRNot "D1" "D3"
                          ]
                    , tDecls
                          [ "rec datatype D1 of C1 D1 D2 D3; subtype datatype D2 of C2 D1 D2 D3; subtype datatype D3 of C3 D1 D2 D3; C4; end; end; end; end"
                          ] $
                      tGroup
                          "recursive"
                          [ testExpectSuccess "pass"
                          , testExpectSuccess $ isOfType "C1 C4 C4 C4" "D1"
                          , testExpectReject $ isOfType "C1 C4 C4 C4" "D2"
                          , testExpectReject $ isOfType "C1 C4 C4 C4" "D3"
                          , testExpectSuccess $ isOfType "C2 C4 C4 C4" "D1"
                          , testExpectSuccess $ isOfType "C2 C4 C4 C4" "D2"
                          , testExpectReject $ isOfType "C2 C4 C4 C4" "D3"
                          , testExpectSuccess $ isOfType "C3 C4 C4 C4" "D1"
                          , testExpectSuccess $ isOfType "C3 C4 C4 C4" "D2"
                          , testExpectSuccess $ isOfType "C3 C4 C4 C4" "D3"
                          , subtypeTest False SRSingle "D2" "D1"
                          , subtypeTest False SRSingle "D3" "D2"
                          , subtypeTest False SRSingle "D3" "D1"
                          , subtypeTest False SRNot "D1" "D2"
                          , subtypeTest False SRNot "D2" "D3"
                          , subtypeTest False SRNot "D1" "D3"
                          , tGroup
                                "GDS"
                                [ testExpectSuccess $ isOfType "match C1 _ _ _ => () end" "D1 -> Unit"
                                , testExpectSuccess $ isOfType "match C2 _ _ _ => () end" "D1 -> Unit"
                                , testExpectSuccess $ isOfType "match C3 _ _ _ => () end" "D1 -> Unit"
                                , testExpectSuccess $ isOfType "match C4 => () end" "D1 -> Unit"
                                ]
                          ]
                    , testExpectSuccess
                          "let rec datatype L of LNil; subtype datatype L1 of LCons Unit L end end end in pass"
                    , testExpectSuccess
                          "let rec datatype L +a of LNil; subtype datatype L1 of LCons a (L a) end end end in pass"
                    ]
              , tGroup
                    "record-constructor"
                    [ tGroup
                          "rank-1"
                          [ tDecls ["datatype R of MkR of di: Integer end end"] $
                            tGroup
                                "one"
                                [ testExpectSuccess "pass"
                                , testExpectSuccess "testeq {22} {(let di = 22 in MkR) >- fn MkR => di}"
                                , testExpectSuccess
                                      "let f: Integer -> R = fn di => MkR; g: R -> Integer = fn MkR => di; in testeq {17} {g $ f 17}"
                                ]
                          , tDecls ["datatype R of MkR of di: Integer; dt: Text end end"] $
                            tGroup
                                "two"
                                [ testExpectSuccess "pass"
                                , testExpectSuccess
                                      "let f: Integer -> R = fn di => let dt = \"t\" in MkR; g: R -> Integer = fn MkR => di; in testeq {17} {g $ f 17}"
                                , testExpectSuccess
                                      "let f: Integer -> R = fn di => let dt = \"t\" in MkR; g: R -> Text = fn MkR => dt; in testeq {\"t\"} {g $ f 17}"
                                ]
                          ]
                    , tDecls
                          [ "datatype R of MkR of df: (a -> a) -> a -> a end end"
                          , "twice = fns f x => f (f x)"
                          , "addone: Integer -> Integer = fn x => x + 1"
                          ] $
                      tGroup
                          "rank-2"
                          [ testExpectSuccess "pass"
                          , testExpectSuccess
                                "let r:R = let df: (b -> b) -> b -> b = twice in MkR in r >- fn MkR => testeq {9} {df addone 7}"
                          , testExpectReject
                                "let r:R = let df: (Integer -> Integer) -> Integer -> Integer = twice in MkR in r >- fn MkR => testeq {9} {df addone 7}"
                          ]
                    , tDecls
                          [ "datatype Rec +a of MkRec of rval: rec r. Maybe (a *: r) end end"
                          , "rec1: a -> Rec a = fn x0 => let rval = Just (x0,Nothing) in MkRec"
                          , "rec3: a -> a -> a -> Rec a = fns x0 x1 x2 => let rval = Just (x0,Just (x1,Just (x2,Nothing))) in MkRec"
                          , "rec rShow: (rec r. Maybe (Showable *: r)) -> Text = match Nothing => \"\"; Just (a,r) => show a <> \",\" <> rShow r end end"
                          , "recShow: Rec Showable -> Text = fn MkRec => rShow rval"
                          ] $
                      tGroup
                          "recursive"
                          [ testExpectSuccess "pass"
                          , testExpectSuccess
                                "let r1: Rec Integer = rec3 12 10 57; r2: Rec Showable = r1 in testeq {\"12,10,57\"} {recShow r2}"
                          ]
                    ]
              ]
        , tGroup
              "subtype-decl"
              [ tDecls ["datatype T of T1 Integer end", "unT1 = fn T1 x => x"] $
                tGroup
                    "simple"
                    [ tDecls ["subtype Integer <: T = T1"] $
                      tGroup
                          "Integer <: T"
                          [ testExpectSuccess "pass"
                          , subtypeTest False SRSingle "Integer" "T"
                          , subtypeTest False SRNot "T" "Integer"
                          ]
                    , tDecls ["subtype T <: Integer = unT1"] $
                      tGroup
                          "T <: Integer"
                          [ testExpectSuccess "pass"
                          , subtypeTest False SRNot "Integer" "T"
                          , subtypeTest False SRSingle "T" "Integer"
                          ]
                    ]
              , tDecls ["datatype T +a of T1 (Maybe a) end", "unT1 = fn T1 x => x"] $
                tGroup
                    "parameter"
                    [ tDecls ["subtype Maybe Integer <: T Integer = T1"] $
                      tGroup
                          "plain"
                          [ testExpectSuccess "pass"
                          , subtypeTest False SRSingle "Maybe Integer" "T Integer"
                          , testExpectSuccess "testeq {Just 3} {unT1 $ Just 3}"
                          ]
                    , tDecls ["subtype Maybe a <: T a = T1"] $
                      tGroup
                          "tyvar"
                          [ testExpectSuccess "pass"
                          , subtypeTest False SRSingle "Maybe Integer" "T Integer"
                          , testExpectSuccess "testeq {Just 3} {unT1 $ Just 3}"
                          ]
                    ]
              , tDecls ["datatype T +a of T1 (Maybe a) end", "unT1 = fn T1 x => x"] $
                tGroup
                    "dependent"
                    [ testExpectSuccess $
                      "let x = 17; f = let subtype Unit <: T Integer = fn () => T1 (Just x) in unT1 () in testeq {Just 17} {f}"
                    , testExpectSuccess $
                      "let rec f = let subtype Unit <: T Integer = fn () => T1 (Just x) in unT1 (); x = 17 end in testeq {Just 17} {f}"
                    , testExpectSuccess $
                      "let f = fn x => let subtype Unit <: T Integer = fn () => T1 (Just x) in unT1 () in testeq {Just 17} {f 17}"
                    , testExpectSuccess $
                      "let f = fn x => let y = x; subtype Unit <: T Integer = fn () => T1 (Just y) in unT1 () in testeq {Just 17} {f 17}"
                    ]
              ]
        , tDecls ["closedtype T of T1 Text Number !\"T.T1\"; T2 !\"T.T2\"; T3 Boolean !\"T.T3\" end"] $
          tGroup
              "closedtype"
              [ testExpectSuccess "pass"
              , testExpectSuccess "let f: T -> Entity = fn x => x in pass"
              , testExpectSuccess "let t1 = T1 \"hello\" 3 in pass"
              , testExpectSuccess "let f = fn T1 x _ => x in pass"
              , testExpectSuccess "T1 \"hello\" 3 >- match T1 \"hello\" 3 => pass end"
              , testExpectSuccess
                    "T1 \"hello\" 3 >- match T2 => fail \"T2\"; T1 \"hello\" 2 => fail \"T1 2\"; T1 \"hell\" 3 => fail \"T1 hell\"; T1 \"hello\" 3 => pass end"
              , testExpectSuccess "let closedtype P of end in pass"
              , testExpectSuccess "let closedtype P of P1 !\"P1\" end in pass"
              , testExpectSuccess "let closedtype P of P1 !\"P1\"; end in pass"
              , testExpectSuccess "let closedtype P of P1 Integer !\"P1\" end in pass"
              , testExpectSuccess "let closedtype P of P1 Integer !\"P1\"; end in pass"
              , testExpectSuccess "let closedtype P of P1 Integer !\"P1\"; P2 Text !\"P2\" end in pass"
              , testExpectSuccess "let closedtype P of P1 Integer !\"P1\"; P2 Text !\"P2\"; end in pass"
              , tGroup
                    "nominal"
                    [ testExpectSuccess "let closedtype P of P1 !\"P1\" end; f : P -> P = fn x => x in pass"
                    , testExpectReject
                          "let closedtype P of P1 !\"P1\" end; closedtype Q of end; f : P -> Q = fn x => x in pass"
                    , testExpectReject
                          "let closedtype P of end; closedtype Q of Q1 !\"Q1\" end; f : P -> Q = fn x => x in pass"
                    , testExpectReject "let closedtype P of end; closedtype Q of end; f : P -> Q = fn x => x in pass"
                    , testExpectReject
                          "let closedtype P of P1 !\"P1\" end; closedtype Q of Q1 !\"Q1\" end; f : P -> Q = fn x => x in pass"
                    , testExpectReject
                          "let closedtype P of P1 Integer !\"P1\" end; closedtype Q of Q1 Integer !\"Q1\" end; f : P -> Q = fn x => x in pass"
                    ]
              , tGroup
                    "parameters"
                    [ testExpectSuccess
                          "let closedtype P +a of P1 !\"P1\" end; f : P Integer -> P Integer = fn x => x in pass"
                    , testExpectReject
                          "let closedtype P -a of P1 !\"P1\" end; f : P Integer -> P Integer = fn x => x in pass"
                    , testExpectReject
                          "let closedtype P a of P1 !\"P1\" end; f : P Integer -> P Integer = fn x => x in pass"
                    , testExpectSuccess
                          "let closedtype P +a of P1 a !\"P1\" end; f : P Integer -> P Integer = fn x => x in pass"
                    , testExpectSuccess
                          "let closedtype P +a of P1 a !\"P1\" end; f : P Integer -> Integer= fn P1 x => x in pass"
                    , testExpectSuccess "let closedtype P of P1 !\"P1\" end; f : P -> Entity = fn x => x in pass"
                    , testExpectSuccess
                          "let closedtype P +a of P1 a !\"P1\" end; f : P Entity -> Entity = fn x => x in pass"
                    , testExpectSuccess
                          "let closedtype P +a +b of P1 a b !\"P1\" end; f : P Entity Entity -> Entity = fn x => x in pass"
                    ]
              , tGroup
                    "recursive"
                    [ testExpectSuccess
                          "let closedtype P of P1 !\"P1\" end in let closedtype Q of Q1 P !\"Q1\" end in pass"
                    , testExpectSuccess "let closedtype P of P1 !\"P1\" end; closedtype Q of Q1 P !\"Q1\" end in pass"
                    , testExpectSuccess
                          "let rec closedtype P of P1 !\"P1\" end; closedtype Q of Q1 P !\"Q1\" end end in pass"
                    , testExpectSuccess "let rec closedtype P of P1 Q !\"P1\" end; closedtype Q of end end in pass"
                    , testExpectSuccess
                          "let rec closedtype P of P1 Q !\"P1\" end; closedtype Q of Q1 P !\"Q1\" end end in pass"
                    , testExpectSuccess "let rec closedtype P of P1 P !\"P1\" end end in pass"
                    , testExpectSuccess "let rec closedtype P +a of P1 (P (a *: a)) !\"P1\" end end in pass"
                    , tDecls
                          [ "rec closedtype L +a of Nil !\"Nil\"; Cons a (L a) !\"Cons\" end end"
                          , "rec listToL: List a -> L a = match [] => Nil; x::xs => Cons x (listToL xs) end end"
                          , "rec lToList: L a -> List a = match Nil => []; Cons x xs => x :: lToList xs end end"
                          ] $
                      tGroup
                          "list"
                          [ testExpectSuccess "pass"
                          , testExpectSuccess "let l = listToL [1,2,3] in pass"
                          , testExpectSuccess "let l = listToL [1,2,3] in testeq {lToList l} {[1,2,3]}"
                          , testExpectSuccess "testeq {lToList $ listToL [1,2,3]} {[1,2,3]}"
                          , testExpectSuccess "testeq {listToL [1,2,3]} {Cons 1 (Cons 2 (Cons 3 Nil))}"
                          , testExpectSuccess "testeq {lToList $ Cons 1 $ Cons 2 $ Cons 3 Nil} {[1,2,3]}"
                          ]
                    ]
              ]
        , tGroup
              "type escape"
              [ testExpectSuccess
                    "let opentype T; t = let in openEntity @T !\"t\"; f = let f : T -> Action Unit = fn _ => pass in f; in f t"
              , testExpectReject
                    "let opentype T1; opentype T2; t = let in openEntity @T1 !\"t\"; f = let f : T2 -> Action Unit = fn _ => pass in f; in f t"
              , testExpectReject
                    "let t = let opentype T in openEntity @T !\"t\"; f = let opentype T; f : T -> Action Unit = fn _ => pass in f; in f t"
              , testExpectReject
                    "let t = let opentype T1 in openEntity @T1 !\"t\"; f = let opentype T2; f : T2 -> Action Unit = fn _ => pass in f; in f t"
              ]
        , tGroup
              "general-subtype"
              [ testExpectReject "let subtype Unit <: Unit = fn _ => () in pass"
              , testExpectReject "let subtype Integer <: Unit = fn _ => () in pass"
              , tDecls ["opentype P", "opentype Q"] $
                tGroup
                    "opentype"
                    [ testExpectSuccess "let subtype P <: P in pass"
                    , testExpectSuccess "let subtype P <: Q in pass"
                    , testExpectReject "let subtype P <: P = fn x => x in pass"
                    , testExpectReject "let subtype P <: Q = fn _ => error \"\" in pass"
                    ]
              , tDecls ["datatype P of MkP Number end"] $
                tGroup
                    "datatype"
                    [ testExpectSuccess "let subtype Number <: P = MkP in pass"
                    , testExpectReject "let subtype Number <: P = MkP; subtype Number <: P = MkP in pass"
                    , testExpectReject "let subtype Number <: P = MkP; subtype Integer <: P = MkP in pass"
                    ]
              , tDecls
                    [ "datatype A of MkA Number end"
                    , "datatype B of MkB Number end"
                    , "datatype C of MkC Number end"
                    , "datatype D of MkD Number end"
                    , "subtype A <: B = fn MkA x => MkB x"
                    , "subtype C <: D = fn MkC x => MkD x"
                    , "subtype A <: D = fn MkA x => MkD x"
                    ] $
                tGroup
                    "verify"
                    [ testExpectSuccess "pass"
                    , testExpectReject "let subtype B <: B = fn MkB x => MkB x in pass"
                    , testExpectReject "let subtype B <: C = fn MkB x => MkC x in pass"
                    ]
              , tDecls ["datatype A of MkA Number end", "datatype B of MkB Number end"] $
                tGroup
                    "trustme"
                    [ testExpectSuccess "pass"
                    , testExpectSuccess "let subtype A <: B = fn MkA x => MkB x in pass"
                    , testExpectReject
                          "let subtype A <: B = fn MkA x => MkB x; subtype A <: B = fn MkA x => MkB x in pass"
                    , testExpectSuccess
                          "let subtype A <: B = fn MkA x => MkB x; subtype trustme A <: B = fn MkA x => MkB x in pass"
                    , testExpectSuccess
                          "let subtype trustme A <: B = fn MkA x => MkB x; subtype A <: B = fn MkA x => MkB x in pass"
                    ]
              , tDecls ["datatype A +x of MkA x end", "datatype B +x of MkB x end", "datatype C of MkC end"] $
                tGroup
                    "preferred"
                    [ testExpectSuccess "pass"
                    , testExpectSuccess "let subtype trustme A Number <: B Number = fn MkA x => MkB x in pass"
                    , tDecls
                          [ "subtype trustme A Number <: B Number = fn MkA x => MkB x"
                          , "subtype trustme B Number <: C = fn MkB _ => MkC"
                          , "subtype trustme A Any <: C = fn MkA _ => MkC"
                          ] $
                      subtypeTest False SRSingle "A Unit" "C"
                    , tDecls
                          [ "subtype trustme A Any <: C = fn MkA _ => MkC"
                          , "subtype trustme A Number <: B Number = fn MkA x => MkB x"
                          , "subtype trustme B Number <: C = fn MkB _ => MkC"
                          ] $
                      subtypeTest False SRSingle "A Unit" "C"
                    , tDecls
                          [ "subtype trustme A a <: B a = fn MkA x => MkB x"
                          , "subtype trustme B Any <: C = fn MkB _ => MkC"
                          , "subtype trustme A Number <: C = fn MkA _ => MkC"
                          ] $
                      subtypeTest False SRSingle "A Unit" "C"
                    , tDecls
                          [ "subtype trustme A Number <: C = fn MkA _ => MkC"
                          , "subtype trustme A a <: B a = fn MkA x => MkB x"
                          , "subtype trustme B Any <: C = fn MkB _ => MkC"
                          ] $
                      subtypeTest False SRSingle "A Unit" "C"
                    , tGroup
                          "order"
                          [ tGroup
                                "var"
                                [ tDecls
                                      [ "subtype trustme A a <: B a = fn MkA x => MkB x"
                                      , "subtype trustme A Number <: B Number = fn MkA x => MkB x"
                                      ] $
                                  subtypeTest False SRSingle "A Unit" "B Unit"
                                , tDecls
                                      [ "subtype trustme A Number <: B Number = fn MkA x => MkB x"
                                      , "subtype trustme A a <: B a = fn MkA x => MkB x"
                                      ] $
                                  subtypeTest False SRSingle "A Unit" "B Unit"
                                ]
                          , tGroup
                                "incoherent"
                                [ tDecls
                                      [ "subtype trustme A Integer <: B Integer = fn MkA x => MkB x"
                                      , "subtype trustme A Number <: B Number = fn MkA x => MkB x"
                                      ] $
                                  tGroup
                                      "1"
                                      [ subtypeTest False SRNot "A Number" "B Number"
                                      , subtypeTest False SRNot "A Integer" "B Integer"
                                      ]
                                , tDecls
                                      [ "subtype trustme A Number <: B Number = fn MkA x => MkB x"
                                      , "subtype trustme A Integer <: B Integer = fn MkA x => MkB x"
                                      ] $
                                  tGroup
                                      "2"
                                      [ subtypeTest False SRNot "A Number" "B Number"
                                      , subtypeTest False SRNot "A Integer" "B Integer"
                                      ]
                                ]
                          , tGroup
                                "duplicate"
                                [ tDecls
                                      [ "subtype trustme A Number <: B Number = fn MkA x => MkB x"
                                      , "subtype trustme A Number <: B Number = fn MkA x => MkB x"
                                      ] $
                                  subtypeTest False SRSingle "A Number" "B Number"
                                ]
                          , tGroup
                                "sub"
                                [ tDecls
                                      [ "subtype trustme A Integer <: B Number = fn MkA x => MkB x"
                                      , "subtype trustme A Number <: B Integer = fn MkA _ => MkB 0"
                                      ] $
                                  subtypeTest False SRSingle "A Number" "B Integer"
                                , tDecls
                                      [ "subtype trustme A Number <: B Integer = fn MkA _ => MkB 0"
                                      , "subtype trustme A Integer <: B Number = fn MkA x => MkB x"
                                      ] $
                                  subtypeTest False SRSingle "A Number" "B Integer"
                                ]
                          , tGroup
                                "in"
                                [ tDecls
                                      [ "subtype trustme A Integer <: C = fn MkA _ => MkC"
                                      , "subtype trustme A Number <: C = fn MkA _ => MkC"
                                      ] $
                                  subtypeTest False SRSingle "A Number" "C"
                                , tDecls
                                      [ "subtype trustme A Number <: C = fn MkA _ => MkC"
                                      , "subtype trustme A Integer <: C = fn MkA _ => MkC"
                                      ] $
                                  subtypeTest False SRSingle "A Number" "C"
                                ]
                          , tGroup
                                "out"
                                [ tDecls
                                      [ "subtype trustme C <: B Integer = fn MkC => MkB 0"
                                      , "subtype trustme C <: B Number = fn MkC => MkB 0"
                                      ] $
                                  subtypeTest False SRSingle "C" "B Integer"
                                , tDecls
                                      [ "subtype trustme C <: B Number = fn MkC => MkB 0"
                                      , "subtype trustme C <: B Integer = fn MkC => MkB 0"
                                      ] $
                                  subtypeTest False SRSingle "C" "B Integer"
                                ]
                          ]
                    , tDecls ["subtype List (A a) <: A (List a) = fn aa => MkA $ mapList (fn MkA a => a) aa"] $
                      tGroup
                          "monoid"
                          [ testExpectSuccess "pass"
                          , subtypeTest False SRSingle "List (A Integer)" "A (List Integer)"
                          , subtypeTest True SRSubsume "List (A None)" "A (List None)"
                          ]
                    ]
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
              [ testExpectSuccess "do r <- newMemWholeModel; interpretIntegerAsText r := \"37\"; testeq {37} r; end"
              , testExpectSuccess
                    "do r <- newMemWholeModel; interpretDateAsText r := \"2015-08-12\"; testeq {YearMonthDay 2015 08 12} r; end"
              ]
        , tDecls
              [ "runresult = fns ar arg => ar >- match Left err => fail err; Right f => f arg end"
              , "testaction = fns expected action => do found <- action; testeqval expected found end"
              , "testleft = fn action => do found <- action; found >- match Left _ => pass; Right _ => fail \"not Left\" end end"
              , "using Eval"
              ] $
          tGroup
              "evaluate"
              [ testExpectSuccess "testaction (Right True) $ evaluate @Boolean \"True\""
              , testExpectSuccess "testaction (Right 5) $ evaluate @Integer \"5\""
              , testExpectSuccess "testaction (Right 5) $ evaluate @Integer \"let x = 5 in x\""
              , testExpectSuccess
                    "do ar <- evaluate @(Integer -> Integer) \"fn x => x + 1\"; ar >- match Left err => fail err; Right f => testeqval 8 $ f 7 end end"
              , testExpectSuccess "testaction (Left \"<evaluate>:1:1: expecting: expression\") $ evaluate @Integer \"\""
              , testExpectSuccess "testaction (Left \"<evaluate>:1:1: undefined: f: a\") $ evaluate @Integer \"f\""
              , testExpectSuccess "testleft $ evaluate @Integer \"\\\"hello\\\"\""
              , testExpectSuccess
                    "do r <- newMemWholeModel; ar <- evaluate @(WholeModel Integer -> Action Unit) \"fn r => r := 45\"; runresult ar r; a <- get r; testeqval 45 a; end"
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
        , tGroup
              "task"
              [ testExpectSuccess
                    "do t <- Task.async $ do sleep $ Seconds 0.01; return True end; v <- Task.await t; if v then pass else fail \"\" end"
              , testExpectSuccess
                    "do r <- newMemWholeModel; r := 0; t <- Task.async $ do sleep $ Seconds 0.01; r := 1; end; Task.await t; v <- get r; if v == 1 then pass else fail \"\" end"
              , testExpectSuccess
                    "do r <- newMemWholeModel; r := 0; t <- Task.async $ do sleep $ Seconds 0.05; r := 1; end; v <- get r; if v == 0 then pass else fail \"\" end"
              , testExpectSuccess
                    "do r <- newMemWholeModel; r := 0; t <- lifecycle $ Task.async $ do sleep $ Seconds 0.05; r := 1; end; v <- get r; if v == 1 then pass else fail \"\" end"
              ]
        ]
