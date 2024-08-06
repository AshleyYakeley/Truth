module Test.Entity
    ( testEntity
    , testUpdates
    ) where

import Changes.Core
import Pinafore.Test.Internal
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
    runScriptTestTree $
    tGroup "update" [testUpdate "do model <- newMem.WholeModel; pure.Action (model :=.WholeModel 1, model) end"]

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
          , testSubtypeUnify polar sr $ "let rec f: (" <> q <> ") -> Unit = f; x: " <> p <> " = x; fx = f x in pass"
          ]
    , tGroup "subsume" $
      [ testSubtypeSubsume polar sr $ "let rec x: " <> p <> " = x; y: " <> q <> " = x in pass"
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
        [ "pass = pure ()"
        , "undefined = error \"undefined\""
        , "runWholeModel = fn r => do a <- get r; a end"
        , "runreforfail = fn r => runWholeModel (r ?? {fail \"unknown model\"})"
        , "testeq = fn expected, found => runreforfail {if %expected == %found then pass else fail \"not equal\"}"
        , "testneq = fn expected, found => runreforfail {if %expected /= %found then pass else fail \"equal\"}"
        , "testisknown = fn t => runWholeModel {if %(known t) then pass else fail \"known\"}"
        , "testisunknown = fn t => runWholeModel {if %(known t) then fail \"known\" else pass}"
        , "testeqval = fn e, f => testeq {e} {f}"
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
              [ testExpectSuccess "do pure () end"
              , testExpectSuccess "do pure (); end"
              , testExpectSuccess "do testeqval 3 3 end"
              , testExpectSuccess "do a <- pure 3; testeqval 3 a end"
              , testExpectSuccess "do a = 3; testeqval 3 a end"
              , testExpectSuccess "do {# comment #} a = 3; testeqval 3 a end"
              , testExpectSuccess "do {#| doc a #} a = 3; testeqval 3 a end"
              , testExpectSuccess "do a <- pure 3; b <- pure $ a +.Integer a; testeqval 6 b end"
              ]
        , tDecls ["flagRef = do r <- newMem.WholeModel; r := False; pure r; end"] $
          tGroup
              "stop"
              [ testExpectSuccess "pure ()"
              , testExpectThrow "fail \"failure\""
              , testExpectSuccess "expectStop stop"
              , testExpectSuccess "expectStop $ do stop; fail \"unstopped\"; end"
              , testExpectSuccess "do a <- onStop (pure 1) (pure 2); testeqval 1 a; end"
              , testExpectSuccess "do a <- onStop (pure 1) stop; testeqval 1 a; end"
              , testExpectThrow "do a <- onStop (pure 1) stop; fail \"unstopped\"; end"
              , testExpectSuccess "do a <- onStop stop (pure 2); testeqval 2 a; end"
              , testExpectThrow "do a <- onStop stop (pure 2); fail \"unstopped\"; end"
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
                    [ testExpectSuccess "expectStop $ do r <- newMem.WholeModel; get r; end"
                    , testExpectSuccess "do r <- newMem.WholeModel; r := 45; a <- get r; testeqval 45 a; end"
                    , testExpectSuccess "do r <- newMem.WholeModel; r := 3; r := 4; a <- get r; testeqval 4 a; end"
                    , testExpectSuccess
                          "do s <- newMem.FiniteSetModel; n <- get $ count.FiniteSetModel s; testeqval 0 n; end"
                    , testExpectSuccess
                          "do s <- newMem.FiniteSetModel; s +=.SetModel 57; n <- get $ count.FiniteSetModel s; testeqval 1 n; end"
                    , testExpectSuccess
                          "do s <- newMem.FiniteSetModel; s -=.SetModel 57; n <- get $ count.FiniteSetModel s; testeqval 0 n; end"
                    , testExpectSuccess
                          "do s <- newMem.FiniteSetModel; s +=.SetModel 57; s -=.SetModel 57; n <- get $ count.FiniteSetModel s; testeqval 0 n; end"
                    , testExpectSuccess
                          "do s <- newMem.FiniteSetModel; s +=.SetModel 57; m54 <- get $ member.SetModel s {54}; m57 <- get $ member.SetModel s {57}; testeqval False m54; testeqval True m57; end"
                    , testExpectSuccess
                          "do s <- newMem.FiniteSetModel; s -=.SetModel 57; m57 <- get $ member.SetModel s {57}; testeqval False m57; end"
                    , testExpectSuccess
                          "do s <- newMem.FiniteSetModel; s +=.SetModel 57; s -=.SetModel 57; m57 <- get $ member.SetModel s {57}; testeqval False m57; end"
                    , testExpectSuccess
                          "do s <- newMem.FiniteSetModel; member.SetModel s {57} := True; m54 <- get $ member.SetModel s {54}; m57 <- get $ member.SetModel s {57}; testeqval False m54; testeqval True m57; end"
                    , testExpectSuccess
                          "do s <- newMem.FiniteSetModel; member.SetModel s {57} := False; m57 <- get $ member.SetModel s {57}; testeqval False m57; end"
                    , testExpectSuccess
                          "do s <- newMem.FiniteSetModel; member.SetModel s {57} := True; member.SetModel s {57} := False; m57 <- get $ member.SetModel s {57}; testeqval False m57; end"
                    , testExpectSuccess "expectStop $ do r <- newMem.WholeModel; immut.WholeModel r := 5; end"
                    ]
              , tDecls
                    [ "showVal: Showable -> Action Unit = fn v => message.Debug $ show v"
                    , "showList: List Showable -> Action Unit = fn l => do message.Debug \"[[[\"; for_ l showVal;  message.Debug \"]]]\"; end"
                    , "testImmutList = fn present, n, call => do lr <- newMem.ListModel; lr := [10,20,30]; r <- item.ListModel present n lr; ir <- item.ListModel present n $ immut.ListModel lr; call lr; a <- get r; ia <- get ir; testeqval a ia; end"
                    ] $
                tGroup
                    "list"
                    [ testExpectSuccess "pass"
                    , testExpectSuccess "do r <- newMem.ListModel; n <- getCount.ListModel r; testeqval 0 n; end"
                    , testExpectSuccess
                          "do r <- newMem.ListModel; r := [10,20,30]; n <- getCount.ListModel r; testeqval 3 n; end"
                    , testExpectSuccess "do r <- newMem.ListModel; n <- get $ count.ListModel r; testeqval 0 n; end"
                    , testExpectSuccess
                          "do r <- newMem.ListModel; r := [10,20,30]; n <- get $ count.ListModel r; testeqval 3 n; end"
                    , testExpectSuccess
                          "do r <- newMem.ListModel; r := [10,20,30]; ir <- item.ListModel True 1 r; i <- get ir; testeqval 20 i; end"
                    , testExpectSuccess
                          "do r <- newMem.ListModel; r := [10,20,30]; ir <- item.ListModel True 1 r; ir := 25; i <- get ir; testeqval 25 i; end"
                    , testExpectSuccess
                          "do r <- newMem.ListModel; r := [10,20,30]; ir <- item.ListModel True 1 r; ir := 25; l <- get r; testeqval [10,25,30] l; end"
                    , testExpectSuccess
                          "do r <- newMem.ListModel; r := [10,20,30]; ir <- item.ListModel True 1 r; delete ir; l <- get r; testeqval [10,30] l; end"
                    , testExpectSuccess
                          "do r <- newMem.ListModel; r := [10,20,30]; ir <- item.ListModel True 1 r; delete ir; ir := 15; l <- get r; testeqval [10,15,30] l; end"
                    , testExpectSuccess
                          "do r <- newMem.ListModel; r := [10,20,30]; ir <- item.ListModel False 1 r; i <- expectStop $ get ir; pure (); end"
                    , testExpectSuccess
                          "do r <- newMem.ListModel; r := [10,20,30]; ir <- item.ListModel False 1 r; ir := 25; i <- get ir; testeqval 25 i; end"
                    , testExpectSuccess
                          "do r <- newMem.ListModel; r := [10,20,30]; ir <- item.ListModel False 1 r; ir := 25; l <- get r; testeqval [10,25,20,30] l; end"
                    , testExpectSuccess
                          "do r <- newMem.ListModel; r := [10,20,30]; ir <- item.ListModel False 1 r; delete ir; l <- get r; testeqval [10,20,30] l; end"
                    , testExpectSuccess
                          "do r <- newMem.ListModel; r := [10,20,30]; ir <- item.ListModel False 1 r; delete ir; ir := 15; l <- get r; testeqval [10,15,20,30] l; end"
                    , testExpectSuccess
                          "do r <- newMem.ListModel; r := [10,20,30]; ir <- item.ListModel False 1 r; delete ir; l <- get r; testeqval [10,20,30] l; end"
                    , testExpectSuccess
                          "do r <- newMem.ListModel; r := [10,20,30]; ir <- item.ListModel False 1 r; delete ir; ir := 15; l <- get r; testeqval [10,15,20,30] l; end"
                    , testExpectSuccess
                          "do r <- newMem.ListModel; r := [10,20,30]; ir <- item.ListModel True 1 r; insert.ListModel 1 12 r; i <- get ir; testeqval 20 i; end"
                    , testExpectSuccess
                          "do r <- newMem.ListModel; r := [10,20,30]; ir <- item.ListModel True 1 r; insert.ListModel 1 12 r; ir := 15; l <- get r; testeqval [10,12,15,30] l; end"
                    , testExpectSuccess "testImmutList True 1 $ fn _ => pure ()"
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
                          , "toTime.LocalTime -480 $ DateAndTime (YearMonthDay 2022 01 16) (HourMinuteSecond 19 07 22)")
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
              , tGroup
                    "show"
                    [ testExpectSuccess "testeq {\"False\"} {show False}"
                    , testExpectSuccess "testeq {\"34\"} {show 34}"
                    , testExpectSuccess "testeq {\"\\\"hello\\\"\"} {show \"hello\"}"
                    ]
              ]
        , tOpenDefaultStore $
          tWith ["Store"] $
          tDecls
              [ "opentype E"
              , "eea = property @E @E !\"eea\" store"
              , "eeb = property @E @E !\"eeb\" store"
              , "eec = property @E @E !\"eec\" store"
              , "eed = property @E @E !\"eed\" store"
              , "eta = property @E @Text !\"eta\" store"
              , "eia = property @E @Integer !\"eia\" store"
              , "eib = property @E @Integer !\"eib\" store"
              , "eic = property @E @Integer !\"eic\" store"
              , "tea = property @Text @E !\"tea\" store"
              , "nea = property @Integer @E !\"nea\" store"
              , "e1 = point.OpenEntity @E !\"e1\""
              , "e2 = point.OpenEntity @E !\"e2\""
              , "e3 = point.OpenEntity @E !\"e3\""
              , "e4 = point.OpenEntity @E !\"e4\""
              , "eba = property @E @Boolean !\"eba\" store"
              , "era = property @E @Rational !\"era\" store"
              , "ena = property @E @Number !\"ena\" store"
              ] $
          tGroup
              "storage"
              [ testExpectSuccess "pass"
              , tGroup
                    "unknown & known"
                    [ testExpectSuccess "testisunknown {% (eta !$ {e1}) == % (eta !$ {e1})}"
                    , testExpectSuccess "runreforfail {if %(known unknown) then fail \"failed\" else pass}"
                    , testExpectSuccess "runreforfail {if %(known $ eta !$ {e1}) then fail \"failed\" else pass}"
                    , testExpectSuccess
                          "pass >> runreforfail {if %(known $ eta !$ {e1}) then fail \"failed\" else pass}"
                    , testExpectSuccess
                          "runreforfail {pass >>.Action if %(known $ eta !$ {e1}) then fail \"failed\" else pass}"
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
                    , testExpectSuccess "tea !$ {\"hello\"} := e1 >> testeq {1} (count.FiniteSetModel (tea !@ {e1}))"
                    , testExpectSuccess "(eea ..Property eea) !$ {e1} := e2"
                    , testExpectSuccess
                          "do (eea ..Property eea) !$ {e1} := e2; testeq {e2} ((eea ..Property eea) !$ {e1}); testeq {e2} (eea !$ (eea !$ {e1})); end"
                    , testExpectSuccess
                          "do eea !$ (eea !$ {e1}) := e2; testeq {e2} ((eea ..Property eea) !$ {e1}); testeq {e2} (eea !$ (eea !$ {e1})); end"
                    , testExpectSuccess "expectStop $ do r <- newMem.WholeModel; eia !$ r := 4; end"
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
                    "clear.FiniteSetModel"
                    [ testExpectSuccess
                          "eta !@ {\"hello\"} += e1 >> clear.FiniteSetModel (eta !@ {\"hello\"}) >> testisunknown (eta !$ {e1})"
                    ]
              , tDecls ["c1 = cell @Boolean !\"c1\" store"] $
                tGroup
                    "cell"
                    [ testExpectSuccess "c1 := True >> testeq {True} c1"
                    , testExpectSuccess "c1 := False >> testeq {False} c1"
                    ]
              , tDecls ["s1 = set @Integer !\"s1\" store"] $
                tGroup
                    "set"
                    [ testExpectSuccess "testeq {[]} (toList.FiniteSetModel order.Integer s1)"
                    , testExpectSuccess "s1 += 2 >> testeq {[2]} (toList.FiniteSetModel order.Integer s1)"
                    , testExpectSuccess "s1 += 5 >> s1 += 3 >> testeq {[3,5]} (toList.FiniteSetModel order.Integer s1)"
                    ]
              , tGroup
                    "type"
                    [ testExpectSuccess "let c = cell @Boolean !\"c\" store in pass"
                    , testExpectSuccess "let c = cell @Unit !\"c\" store in pass"
                    , testExpectSuccess "let c = cell @(Maybe Integer) !\"c\" store in pass"
                    , testExpectSuccess "let c = cell @(List Unit) !\"c\" store in pass"
                    , testExpectSuccess "let c = cell @(List (List (List Integer))) !\"c\" store in pass"
                    ]
              , tDecls
                    [ "longtext = \"jfkljgkljrklgjkvbnvleriirejgioerjhgitrklnmbdfmkl;dmnverireigjerkgjrevjkrljvkljvklsjvroejrgiojgireojg\""
                    , "ela = property @E @(List Integer) !\"ela\" store"
                    ] $
                tGroup
                    "fetch"
                    [ testExpectSuccess "pass"
                    , testExpectSuccess "do v <- fetch @Text store \"hvfjkhvjrkes\"; testeq {\"hvfjkhvjrkes\"} {v} end"
                    , testExpectSuccess "expectStop $ do v <- fetch @Text store longtext; testeq {longtext} {v} end"
                    , testExpectSuccess
                          "do eta !$ {e1} := longtext; v <- fetch @Text store longtext; testeq {longtext} {v} end"
                    , testExpectSuccess "expectStop $ do v <- fetch @Text store [3,4,5]; testeq {[3,4,5]} {v} end"
                    , testExpectSuccess "do ela !$ {e1} := [3,4,5]; v <- get $ ela !$ {e1}; testeq {[3,4,5]} {v} end"
                    , testExpectSuccess
                          "do ela !$ {e1} := [3,4,5]; v <- fetch @(List Integer) store [3,4,5]; testeq {[3,4,5]} {v} end"
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
                          , testExpectSuccess "ena !$ {e1} := (0 /.Number 0) >> testeq {0 /.Number 0} (ena !$ {e1})"
                          , testExpectSuccess "ena !$ {e1} := (1 /.Number 0) >> testeq {1 /.Number 0} (ena !$ {e1})"
                          , testExpectSuccess "ena !$ {e1} := (-1 /.Number 0) >> testeq {-1 /.Number 0} (ena !$ {e1})"
                          ]
                    ]
              , tGroup
                    "matching literals"
                    [ testExpectSuccess
                          "eta !$ {e1} := \"hello\" >> eta !$ {e2} := \"hello\" >> testeq (eta !$ {e1}) (eta !$ {e2})"
                    ]
              , tGroup
                    "identity property"
                    [ testExpectSuccess "(id.Property !$ eea !$ {e1}) := e2 >> testeq {e2} (eea !$ {e1})"
                    , testExpectSuccess "(eea !$ id.Property !$ {e1}) := e2 >> testeq {e2} (eea !$ {e1})"
                    , testExpectSuccess "((id.Property ..Property eea) !$ {e1}) := e2 >> testeq {e2} (eea !$ {e1})"
                    , testExpectSuccess "((eea ..Property id.Property) !$ {e1}) := e2 >> testeq {e2} (eea !$ {e1})"
                    , testExpectSuccess "eea !$ {e1} := e2 >> testeq {e2} (id.Property !$ eea !$ {e1})"
                    , testExpectSuccess "eea !$ {e1} := e2 >> testeq {e2} (eea !$ id.Property !$ {e1})"
                    , testExpectSuccess "eea !$ {e1} := e2 >> testeq {e2} ((id.Property ..Property eea) !$ {e1})"
                    , testExpectSuccess "eea !$ {e1} := e2 >> testeq {e2} ((eea ..Property id.Property) !$ {e1})"
                    , testExpectSuccess "(id.Property !$ eea !$ {e1}) := e2 >> testeq {e2} (id.Property !$ eea !$ {e1})"
                    ]
              , tGroup
                    "identity inverse property"
                    [ testExpectSuccess "(id.Property !@@ eta !@ {\"hello\"}) += e1 >> testeq {\"hello\"} (eta !$ {e1})"
                    , testExpectSuccess "(eea !@@ id.Property !@ {e2}) += e1 >> testneq {e2} (eea !$ {e1})"
                    , testExpectSuccess "(eta !@ {\"hello\"}) += e1 >> testeq {\"hello\"} (eta !$ {e1})"
                    , testExpectSuccess
                          "((id.Property ..Property eta) !@ {\"hello\"}) += e1 >> testeq {\"hello\"} (eta !$ {e1})"
                    , testExpectSuccess
                          "((eta ..Property id.Property) !@ {\"hello\"}) += e1 >> testeq {\"hello\"} (eta !$ {e1})"
                    , testExpectSuccess
                          "eta !@ {\"hello\"} += e1 >> eta !@ {\"hello\"} -= e1 >> testisunknown (eta !$ {e1})"
                    , testExpectSuccess
                          "eta !@ {\"hello\"} += e1 >> (id.Property !@@ eta !@ {\"hello\"}) -= e1 >> testeq {\"hello\"} (eta !$ {e1})"
                    , testExpectSuccess "eea !@ {e2} += e1 >> testeq {e2} (eea !$ {e1})"
                    , testExpectSuccess
                          "eea !@ {e2} += e1 >> (eea !@@ id.Property !@ {e2}) -= e1 >> testneq {e2} (eea !$ {e1})"
                    , testExpectSuccess
                          "eta !@ {\"hello\"} += e1 >> ((id.Property ..Property eta) !@ {\"hello\"}) -= e1 >> testisunknown (eta !$ {e1})"
                    , testExpectSuccess
                          "eta !@ {\"hello\"} += e1 >> ((eta ..Property id.Property) !@ {\"hello\"}) -= e1 >> testisunknown (eta !$ {e1})"
                    ]
              , tGroup
                    "composed properties"
                    [ testExpectSuccess "(eea !$ eeb !$ {e1}) := e2 >> testeq {e2} (eea !$ eeb !$ {e1})"
                    , testExpectSuccess "(eta !$ eeb !$ {e1}) := \"hello\" >> testeq {\"hello\"} (eta !$ eeb !$ {e1})"
                    , testExpectSuccess "(eea ..Property eeb !$ {e1}) := e2 >> testeq {e2} (eea !$ eeb !$ {e1})"
                    , testExpectSuccess
                          "(eta ..Property eeb !$ {e1}) := \"hello\" >> testeq {\"hello\"} (eta !$ eeb !$ {e1})"
                    , testExpectSuccess "(eea !$ eeb !$ {e1}) := e2 >> testeq {e2} (eea ..Property eeb !$ {e1})"
                    , testExpectSuccess
                          "(eta !$ eeb !$ {e1}) := \"hello\" >> testeq {\"hello\"} (eta ..Property eeb !$ {e1})"
                    , testExpectSuccess "(eeb ..Property eea) !$ {e2} := e1 >> testeq {e1} (eeb !$ eea !$ {e2})"
                    ]
              , tGroup
                    "composed inverse properties"
                    [ testExpectSuccess "(eeb !@@ eta !@ {\"hello\"}) += e1 >> testeq {\"hello\"} (eta !$ eeb !$ {e1})"
                    , testExpectSuccess
                          "((eta ..Property eeb) !@ {\"hello\"}) += e1 >> testeq {\"hello\"} (eta !$ eeb !$ {e1})"
                    , testExpectSuccess "((eta ..Property eeb) !@ {\"hello\"}) += e1 >> testisunknown (eta !$ {e1})"
                    , testExpectSuccess
                          "eeb !$ {e1} := e2 >> ((eta ..Property eeb) !@ {\"hello\"}) += e1 >> testeq {e2} (eeb !$ {e1})"
                    , testExpectSuccess
                          "eeb !$ {e1} := e2 >> ((eta ..Property eeb) !@ {\"hello\"}) += e1 >> testeq {\"hello\"} (eta !$ {e2})"
                    , testExpectSuccess
                          "eeb !$ {e1} := e2 >> (eeb !@@ eta !@  {\"hello\"}) += e1 >> testeq {e2} (eeb !$ {e1})"
                    , testExpectSuccess
                          "eeb !$ {e1} := e2 >> (eeb !@@ eta !@  {\"hello\"}) += e1 >> testeq {\"hello\"} (eta !$ {e2})"
                    , testExpectSuccess
                          "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> ((eta ..Property eeb) !@ {\"hello\"}) -= e1 >> testeq {e2} (eeb !$ {e1})"
                    , testExpectSuccess
                          "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> ((eta ..Property eeb) !@ {\"hello\"}) -= e1 >> testisunknown (eta !$ {e2})"
                    , testExpectSuccess
                          "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> (eeb !@@ eta !@ {\"hello\"}) -= e1 >> testneq {e2} (eeb !$ {e1})"
                    , testExpectSuccess
                          "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> (eeb !@@ eta !@ {\"hello\"}) -= e1 >> testeq {\"hello\"} (eta !$ {e2})"
                    , testExpectSuccess
                          "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> clear.FiniteSetModel ((eta ..Property eeb) !@ {\"hello\"}) >> testeq {e2} (eeb !$ {e1})"
                    , testExpectSuccess
                          "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> clear.FiniteSetModel ((eta ..Property eeb) !@ {\"hello\"}) >> testisunknown (eta !$ {e2})"
                    , testExpectSuccess
                          "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> clear.FiniteSetModel (eeb !@@ eta !@ {\"hello\"}) >> testneq {e2} (eeb !$ {e1})"
                    , testExpectSuccess
                          "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> clear.FiniteSetModel (eeb !@@ eta !@ {\"hello\"}) >> testeq {\"hello\"} (eta !$ {e2})"
                    ]
              , tGroup
                    "single.FiniteSetModel"
                    [ testExpectSuccess "testisunknown (single.FiniteSetModel $ eib !$$ eia !@ {0})"
                    , testExpectSuccess
                          "eib !$ {e1} := 1 >> eia !$ {e1} := 0 >> testeq {1} (single.FiniteSetModel $ eib !$$ eia !@ {0})"
                    , testExpectSuccess
                          "eib !$ {e1} := 1 >> eia !$ {e1} := 0 >> eic !$ {e1} := 0 >> testeq {1} (single.FiniteSetModel $ eib !$$ eia !@ {0})"
                    , testExpectSuccess
                          "eib !$ {e1} := 1 >> eia !$ {e1} := 0 >> eia !$ {e1} := 0 >> testeq {1} (single.FiniteSetModel $ eib !$$ eia !@ {0})"
                    , testExpectSuccess
                          "eib !$ {e1} := 1 >> eib !$ {e2} := 2 >> eia !$ {e1} := 0 >> eia !$ {e2} := 0 >> testisunknown (single.FiniteSetModel $ eib !$$ eia !@ {0})"
                    , testExpectSuccess
                          "eib !$ {e1} := 1 >> eib !$ {e2} := 1 >> eia !$ {e1} := 0 >> eia !$ {e2} := 0 >> testeq {1} (single.FiniteSetModel $ eib !$$ eia !@ {0})"
                    ]
              , tGroup
                    "multiple set member"
                    [ testExpectSuccess "testeq {0} (count.FiniteSetModel (tea !@ {e1}))"
                    , testExpectSuccess "eea !$ {e2} := e1 >> testeq {1} (count.FiniteSetModel (eea !@ {e1}))"
                    , testExpectSuccess "eea !@ {e1} += e2 >> testeq {1} (count.FiniteSetModel (eea !@ {e1}))"
                    , testExpectSuccess "tea !$ {\"hello\"} := e1 >> testeq {e1} (tea !$ {\"hello\"})"
                    , testExpectSuccess "tea !@ {e1} += \"hello\" >> testeq {e1} (tea !$ {\"hello\"})"
                    , testExpectSuccess "tea !$ {\"hello\"} := e1 >> testeq {1} (count.FiniteSetModel (tea !@ {e1}))"
                    , testExpectSuccess "tea !@ {e1} += \"hello\" >> testeq {1} (count.FiniteSetModel (tea !@ {e1}))"
                    , testExpectSuccess
                          "tea !@ {e1} += \"hello\" >> tea !@ {e1} += \"hello\" >> testeq {1} (count.FiniteSetModel (tea !@ {e1}))"
                    , testExpectSuccess
                          "tea !@ {e1} += \"h\" >> tea !@ {e1} += \"hello\" >> testeq {2} (count.FiniteSetModel (tea !@ {e1}))"
                    , testExpectSuccess $
                      "let counter = eia !$ {e1};someset = nea !@ {e1} in " <>
                      "counter := 0 >> someset += 1 >> someset += 1 >> (get (toList.FiniteSetModel empty.Order someset) >>= fn pp => for pp $ fn p => runWholeModel {counter := succ.Integer %counter}) >> testeq {1} counter"
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
                          "let enta = property @E @(Maybe Text) !\"enta\" store in enta !$ {e1} := Just \"abc\" >> (testeq {Just \"abc\"} $ enta !$ {e1})"
                    , testExpectSuccess
                          "let enta = property @E @(Maybe Text) !\"enta\" store in enta !$ {e1} := Nothing >> (testeq {Nothing} $ enta !$ {e1})"
                    ]
              , tGroup
                    "List"
                    [ testExpectSuccess
                          "let enta = property @E @(List Text) !\"enta\" store in enta !$ {e1} := [\"abc\", \"def\"] >> (testeq {[\"abc\", \"def\"]} $ enta !$ {e1})"
                    , testExpectSuccess
                          "let enta = property @E @(List Text) !\"enta\" store in enta !$ {e1} := [] >> (testeq {[]} $ enta !$ {e1})"
                    ]
              , tGroup
                    "Pair/Either"
                    [ testExpectSuccess
                          "let enta = property @E @(Number *: Text) !\"enta\" store in enta !$ {e1} := (74,\"hmm\") >> (testneq {(71,\"hmm\")} $ enta !$ {e1})"
                    , testExpectSuccess
                          "let enta = property @E @(Number *: Text) !\"enta\" store in enta !$ {e1} := (74,\"hmm\") >> (testeq {(74,\"hmm\")} $ enta !$ {e1})"
                    , testExpectSuccess
                          "let enta = property @E @(Number +: Text) !\"enta\" store in enta !$ {e1} := Left 74 >> (testneq {Left 73} $ enta !$ {e1})"
                    , testExpectSuccess
                          "let enta = property @E @(Number +: Text) !\"enta\" store in enta !$ {e1} := Left 74 >> (testeq {Left 74} $ enta !$ {e1})"
                    , testExpectSuccess
                          "let enta = property @E @(Number +: Text) !\"enta\" store in enta !$ {e1} := Right \"abc\" >> (testneq {Right \"adbc\"} $ enta !$ {e1})"
                    , testExpectSuccess
                          "let enta = property @E @(Number +: Text) !\"enta\" store in enta !$ {e1} := Right \"abc\" >> (testeq {Right \"abc\"} $ enta !$ {e1})"
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
                          [ subtypeTest False SRSingle "rec a, Maybe a" "rec a, Maybe a"
                          , subtypeTest False SRSingle "rec a, Maybe a" "rec b, Maybe b"
                          , subtypeTest False SRSingle "rec a, Maybe a" "Maybe (rec a, Maybe a)"
                          , subtypeTest False SRSingle "rec a, Maybe a" "Maybe (rec b, Maybe b)"
                          , subtypeTest False SRSingle "Maybe (rec a, Maybe a)" "rec a, Maybe a"
                          , subtypeTest False SRSingle "Maybe (rec a, Maybe a)" "rec b, Maybe b"
                          , subtypeTest False SRSingle "Maybe (rec a, Maybe a)" "Maybe (rec a, Maybe a)"
                          , subtypeTest False SRSingle "Maybe (rec a, Maybe a)" "Maybe (rec b, Maybe b)"
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
              , tDecls
                    ["subtype Map a <: Entity -> Maybe a = fn m, e => lookup.Map e m", "x = single.Map 34 \"sometext\""] $
                tGroup "Map" [testExpectSuccess "pass", testExpectSuccess "testeqval (Just \"sometext\") $ x 34"]
              , tDecls ["opentype Q", "subtype Maybe Number <: Q"] $
                tGroup
                    "non-simple" -- not allowed, per issue #28
                    [testExpectReject "pass"]
              , tDecls ["opentype Q", "subtype Integer <: Q"] $ tGroup "literal" $ strictSubtypeTests "Integer" "Q"
              , tDecls ["opentype Q", "datatype storable P of P1 Text Number !\"P.P1\" end", "subtype P <: Q"] $
                tGroup "closed" $ strictSubtypeTests "P" "Q"
              , tDecls
                    [ "opentype Q"
                    , "opentype R"
                    , "datatype storable P of P1 Text Number !\"P.P1\" end"
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
              , tDecls
                    [ "testIsJust = match Just _ => pass; Nothing => fail \"Nothing\" end"
                    , "testIsNothing = match Nothing => pass; Just _ => fail \"Just\" end"
                    ] $
                tGroup "predicate" $
                [ tDecls ["predicatetype Even <: Integer = fn i => mod i 2 == 0"] $
                  tGroup
                      "Even"
                      [ testExpectSuccess "pass"
                      , tGroup "Even <: Integer" $ strictSubtypeTests "Even" "Integer"
                      , tGroup "Even <: Literal" $ strictSubtypeTests "Even" "Literal"
                      , testExpectSuccess "testeq {Just 4} {check @Even 4}"
                      , testExpectSuccess "testeq {Nothing} {check @Even 5}"
                      ]
                , tDecls ["predicatetype F <: Integer -> Integer = fn f => f 3 == 4"] $
                  tGroup
                      "Integer -> Integer"
                      [ testExpectSuccess "pass"
                      , testExpectSuccess "testIsJust $ check @F $ fn x => x + 1"
                      , testExpectSuccess "testIsNothing $ check @F $ fn x => x"
                      , testExpectSuccess "testeq {Just 6} {map.Maybe (fn f => f 5) $ check @F $ fn x => x + 1}"
                      ]
                , testExpectReject "let predicatetype F <: a -> a = fn f => True in pass"
                , tGroup
                      "unroll"
                      [ testExpectSuccess
                            "let predicatetype AtLeastThree <: Maybe (rec a, Maybe a) = match Just (Just (Just _)) => True; _ => False; end in pass"
                      , testExpectSuccess
                            "let predicatetype AtLeastThree <: rec a, Maybe a = match Just (Just (Just _)) => True; _ => False; end in pass"
                      , testExpectSuccess
                            "let predicatetype AtLeastThree <: rec a, a +: a = match Left (Left (Left _)) => True; _ => False; end in pass"
                      , testExpectSuccess
                            "let predicatetype AtLeastThree <: rec a, rec b, a +: b = match Left (Left (Left _)) => True; _ => False; end in pass"
                      , testExpectReject "let predicatetype Degenerate <: rec a, a = fn _ => True in pass"
                      ]
                , tGroup
                      "storable"
                      [ testExpectSuccess "let predicatetype F <: Integer -> Integer = fn _ => True in pass"
                      , testExpectReject "let predicatetype storable F <: Integer -> Integer = fn _ => True in pass"
                      , tOpenDefaultStore $
                        tWith ["Store"] $
                        tDecls
                            [ "predicatetype storable Even <: Integer = fn i => mod i 2 == 0"
                            , "cellInteger = cell @Integer !\"c\" store"
                            , "cellEven = cell @Even !\"c\" store"
                            ] $
                        tGroup
                            "store"
                            [ testExpectSuccess "pass"
                            , testExpectSuccess "do cellInteger := 18; testeq {18} cellInteger end"
                            , testExpectSuccess "do cellInteger := 18; testeq {18} cellEven end"
                            , testExpectSuccess "do cellInteger := 17; testeq {17} cellInteger end"
                            , testExpectSuccess "do cellInteger := 17; testisunknown cellEven end"
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
              , "dynamictype Q"
              , "subtype P1 <: Q"
              , "subtype P2 <: Q"
              , "e1 = point.DynamicEntity @P1 !\"e1\""
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
              , tDecls
                    [ "dynamictype R"
                    , "subtype Q <: R"
                    , "dynamictype P3 = !\"P3\""
                    , "e3 = point.DynamicEntity @P3 !\"e3\""
                    , "subtype P3 <: Q"
                    ] $
                tGroup
                    "transitive"
                    [ testExpectSuccess "pass"
                    , testExpectSuccess "testeq {Just e1} {check @R e1}"
                    , testExpectSuccess "testeq {Just e3} {check @Q e3}"
                    , testExpectSuccess "testeq {Just e3} {check @R e3}"
                    ]
              ]
        , tDecls
              [ "datatype T of T1 Text Number; T2; T3 Boolean; T4 (WholeModel {-Boolean,+Integer} -> Integer); T5 Text (Boolean -> Integer) end"
              ] $
          tGroup
              "datatype"
              [ testExpectSuccess "pass"
              , testExpectSuccess "let t1 = T1.T \"hello\" 3 in pass"
              , testExpectSuccess "let f = fn T1.T x _ => x in pass"
              , testExpectSuccess "T2.T >- match T2.T => pass end"
              , testExpectSuccess "T3.T True >- match T3.T True => pass end"
              , testExpectSuccess "T1.T \"hello\" 3 >- match T1.T \"hello\" 3 => pass end"
              , testExpectSuccess
                    "T1.T \"hello\" 3 >- match T2.T => fail \"T2.T\"; T1.T \"hello\" 2 => fail \"T1.T 2\"; T1.T \"hell\" 3 => fail \"T1.T hell\"; T1.T \"hello\" 3 => pass end"
              , testExpectSuccess
                    "let f : Boolean -> Integer = fn b => if b then 1 else 0 in T5.T \"abcd\" f >- match T5.T _ ff => if ff True == 1 then pass else fail \"ff\" end"
              , testExpectReject "let datatype B of Mk a end in pass"
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
                    "contain-recursive"
                    [ tDecls
                          [ "datatype P of Mk (rec a, Maybe a) end"
                          , "let rec fromR: (rec a, Maybe a) -> Integer = match Nothing => 0; Just a => 1 + fromR a end end"
                          , "let rec toR: Integer -> (rec a, Maybe a) = match 0 => Nothing; i => Just $ toR (i - 1) end end"
                          , "let rec fromP: P -> Integer = match Mk.P Nothing => 0; Mk.P (Just a) => 1 + fromP (Mk.P a) end end"
                          , "let rec toP: Integer -> P = match 0 => Mk.P Nothing; i => toP (i - 1) >- fn Mk.P a => Mk.P $ Just a end end"
                          ] $
                      tGroup
                          "Maybe"
                          [ testExpectSuccess "pass"
                          , testExpectSuccess "testeqval 5 $ fromP $ Mk.P $ Just $ Just $ Just $ Just $ Just Nothing"
                          , testExpectSuccess "testeqval 17 $ fromP $ toP 17"
                          , testExpectSuccess "testeqval 17 $ fromP $ Mk.P $ toR 17"
                          , testExpectSuccess "testeqval 17 $ fromR $ toP 17 >- fn Mk.P a => a"
                          ]
                    , tDecls
                          [ "datatype Q +t of Mk (rec a, Maybe (t *: a)) end"
                          , "let rec fromR: (rec a, Maybe (t *: a)) -> List t = match Nothing => []; Just (t,a) => t :: fromR a end end"
                          , "let rec toR: List t -> (rec a, Maybe (t *: a)) = match [] => Nothing; t :: tt => Just (t, toR tt) end end"
                          , "let rec fromQ: Q t -> List t = match Mk.Q Nothing => []; Mk.Q (Just (t,a)) => t :: fromQ (Mk.Q a) end end"
                          , "let rec toQ: List t -> Q t = match [] => Mk.Q Nothing; t :: tt => Mk.Q $ Just (t, toQ tt >- fn Mk.Q a => a) end end"
                          ] $
                      tGroup
                          "Pair"
                          [ testExpectSuccess "pass"
                          , testExpectSuccess "testeqval [5,3,1] $ fromQ $ Mk.Q $ Just (5,Just (3,Just (1,Nothing)))"
                          , testExpectSuccess "testeqval [8,12,27,45] $ fromQ $ toQ [8,12,27,45]"
                          , testExpectSuccess "testeqval [8,12,27,45] $ fromQ $ Mk.Q $ toR [8,12,27,45]"
                          , testExpectSuccess "testeqval [8,12,27,45] $ fromR $ toQ [8,12,27,45] >- fn Mk.Q a => a"
                          ]
                    ]
              , tGroup
                    "recursive"
                    [ testExpectSuccess "let datatype P of P1 end in let datatype Q of Q1 P end in pass"
                    , testExpectSuccess "let datatype P of P1 end; datatype Q of Q1 P end in pass"
                    , testExpectSuccess "let rec datatype P of P1 Q end; datatype Q of end in pass"
                    , testExpectSuccess "let rec datatype P of P1 Q end; datatype Q of Q1 P end in pass"
                    , testExpectSuccess "let rec datatype P of P1 P end in pass"
                    , testExpectSuccess
                          "let rec datatype P of P1 Q end; datatype Q of Q1 P end; f : P -> P = match P1.P q => q >- match Q1.Q p => p end end in pass"
                    , testExpectSuccess "let rec datatype P of P1 Q end; datatype storable Q of Q1 !\"Q1\" end in pass"
                    , testExpectReject "let rec datatype storable P of P1 Q end; datatype Q of Q1 !\"Q1\" end in pass"
                    , testExpectSuccess
                          "let rec datatype P of P1 Q end; datatype Q of Q1 (Action Unit) end; pqpass = P1.P (Q1.Q pass) in pqpass >- match P1.P (Q1.Q p) => p end"
                    ]
              , tGroup
                    "parameters"
                    [ tGroup
                          "variance"
                          [ testExpectSuccess "let datatype B +a of Mk a end in pass"
                          , testExpectReject "let datatype B -a of Mk a end in pass"
                          , testExpectSuccess "let datatype B -a of Mk (a -> Boolean) end in pass"
                          , testExpectReject "let datatype B +a of Mk (a -> Boolean) end in pass"
                          , testExpectSuccess "let datatype B {-p,+q} of Mk (p -> q) end in pass"
                          , testExpectSuccess "let datatype B {+q,-p} of Mk (p -> q) end in pass"
                          , testExpectReject "let datatype B {-p,+q} of Mk (q -> p) end in pass"
                          , testExpectReject "let datatype B {+q,-p} of Mk (q -> p) end in pass"
                          ]
                    , tGroup
                          "recursive"
                          [ testExpectSuccess "let rec datatype R +a of Mk (R a) end in pass"
                          , testExpectSuccess "let rec datatype R -a of Mk (R a) end in pass"
                          , testExpectSuccess
                                "let rec datatype R1 +a of MkR1 (R2 a) end; datatype R2 +a of MkR2 (R1 a) end in pass"
                          , testExpectSuccess
                                "let rec datatype R1 -a of MkR1 (R2 a) end; datatype R2 -a of MkR2 (R1 a) end in pass"
                          , testExpectSuccess
                                "let rec datatype R1 +a of MkR1 (R2 a -> Integer) end; datatype R2 -a of MkR2 (R1 a -> Integer) end in pass"
                          ]
                    , tGroup
                          "conversion"
                          [ tDecls
                                [ "datatype D +a of Mk1 (List a); Mk2 (Maybe a) end"
                                , "showD: D Showable -> Text = match Mk1.D aa => show aa; Mk2.D ma => show ma end"
                                , "di: D Integer = Mk1.D [576,469,12]"
                                , "sdi: Text = showD di"
                                ] $
                            testExpectSuccess "if sdi == \"[576,469,12]\" then pass else fail sdi"
                          , tDecls
                                [ "datatype D -a of Mk1 (a -> Integer); Mk2 (a -> a -> Text) end"
                                , "dShow: D Number = Mk2.D $ fn a, b => show a <>.Text \",\" <>.Text show b"
                                , "di: D Integer = dShow"
                                , "showD: a -> D a -> Text = fn a => match Mk1.D ai => show $ ai a; Mk2.D aat => aat a a end"
                                , "sd: Text = showD 356 di"
                                ] $
                            testExpectSuccess "if sd == \"356,356\" then pass else fail sd"
                          , tDecls
                                [ "let rec datatype RList +a of Mk (Maybe (a *: RList a)) end end"
                                , "let rec showRList: RList Showable -> Text = fn Mk.RList rl => rl >- match Nothing => \"\"; Just (a,rla) => show a <>.Text \";\" <>.Text showRList rla end end"
                                , "rlisti: RList Integer = Mk.RList $ Just (45,Mk.RList $ Just (72, Mk.RList $ Just (18,Mk.RList Nothing)))"
                                , "rlists: RList Showable = rlisti"
                                , "sd: Text = showRList rlists"
                                ] $
                            testExpectSuccess "if sd == \"45;72;18;\" then pass else fail sd"
                          ]
                    , tGroup
                          "doubled"
                          [ tDecls ["datatype F a of Mk (a -> a) end", "unF = fn Mk.F f => f"] $
                            tGroup
                                "plain"
                                [ testExpectSuccess "pass"
                                , testExpectSuccess "let f = Mk.F id in testeqval 3 $ unF f 3"
                                , testExpectSuccess "let f = Mk.F show in testeqval \"3\" $ unF f 3"
                                ]
                          , tDecls ["datatype F a of Mk of mf: a -> a end end", "unF = fn Mk.F => mf"] $
                            tGroup
                                "record"
                                [ testExpectSuccess "pass"
                                , testExpectSuccess "let f = Mk.F of mf = id end in testeqval 3 $ unF f 3"
                                , testExpectSuccess "let f = Mk.F of mf = show end in testeqval \"3\" $ unF f 3"
                                ]
                          ]
                    ]
              , tGroup
                    "subtype"
                    [ tDecls
                          [ "datatype D1 of Mk Integer; subtype datatype D2 of Mk; subtype datatype D3 of Mk Boolean; end; end; end"
                          ] $
                      tGroup
                          "T"
                          [ testExpectSuccess "pass"
                          , testExpectSuccess $ isOfType "Mk.D1 3" "D1"
                          , testExpectReject $ isOfType "Mk.D1 3" "D2.D1"
                          , testExpectReject $ isOfType "Mk.D1 3" "D3.D2.D1"
                          , testExpectSuccess $ isOfType "Mk.D2.D1" "D1"
                          , testExpectSuccess $ isOfType "Mk.D2.D1" "D2.D1"
                          , testExpectReject $ isOfType "Mk.D2.D1" "D3.D2.D1"
                          , testExpectSuccess $ isOfType "Mk.D3.D2.D1 True" "D1"
                          , testExpectSuccess $ isOfType "Mk.D3.D2.D1 True" "D2.D1"
                          , testExpectSuccess $ isOfType "Mk.D3.D2.D1 True" "D3.D2.D1"
                          , subtypeTest False SRSingle "D2.D1" "D1"
                          , subtypeTest False SRSingle "D3.D2.D1" "D2.D1"
                          , subtypeTest False SRSingle "D3.D2.D1" "D1"
                          , subtypeTest False SRNot "D1" "D2.D1"
                          , subtypeTest False SRNot "D2.D1" "D3.D2.D1"
                          , subtypeTest False SRNot "D1" "D3.D2.D1"
                          ]
                    , tDeclarator
                          "let rec datatype D1 of Mk D1 D2 D3.D2; subtype datatype D2 of Mk D1 D2 D3; subtype datatype D3 of P D1 D2 D3; Q; end end end" $
                      tGroup
                          "recursive"
                          [ testExpectSuccess "pass"
                          , testExpectSuccess $ isOfType "Mk.D1 Q.D3.D2.D1 Q.D3.D2.D1 Q.D3.D2.D1" "D1"
                          , testExpectReject $ isOfType "Mk.D1 Q.D3.D2.D1 Q.D3.D2.D1 Q.D3.D2.D1" "D2.D1"
                          , testExpectReject $ isOfType "Mk.D1 Q.D3.D2.D1 Q.D3.D2.D1 Q.D3.D2.D1" "D3.D2.D1"
                          , testExpectSuccess $ isOfType "Mk.D2.D1 Q.D3.D2.D1 Q.D3.D2.D1 Q.D3.D2.D1" "D1"
                          , testExpectSuccess $ isOfType "Mk.D2.D1 Q.D3.D2.D1 Q.D3.D2.D1 Q.D3.D2.D1" "D2.D1"
                          , testExpectReject $ isOfType "Mk.D2.D1 Q.D3.D2.D1 Q.D3.D2.D1 Q.D3.D2.D1" "D3.D2.D1"
                          , testExpectSuccess $ isOfType "P.D3.D2.D1 Q.D3.D2.D1 Q.D3.D2.D1 Q.D3.D2.D1" "D1"
                          , testExpectSuccess $ isOfType "P.D3.D2.D1 Q.D3.D2.D1 Q.D3.D2.D1 Q.D3.D2.D1" "D2.D1"
                          , testExpectSuccess $ isOfType "P.D3.D2.D1 Q.D3.D2.D1 Q.D3.D2.D1 Q.D3.D2.D1" "D3.D2.D1"
                          , subtypeTest False SRSingle "D2.D1" "D1"
                          , subtypeTest False SRSingle "D3.D2.D1" "D2.D1"
                          , subtypeTest False SRSingle "D3.D2.D1" "D1"
                          , subtypeTest False SRNot "D1" "D2.D1"
                          , subtypeTest False SRNot "D2.D1" "D3.D2.D1"
                          , subtypeTest False SRNot "D1" "D3.D2.D1"
                          , tGroup
                                "GDS"
                                [ testExpectSuccess $ isOfType "match Mk.D1 _ _ _ => () end" "D1 -> Unit"
                                , testExpectSuccess $ isOfType "match Mk.D2.D1 _ _ _ => () end" "D1 -> Unit"
                                , testExpectSuccess $ isOfType "match P.D3.D2.D1 _ _ _ => () end" "D1 -> Unit"
                                , testExpectSuccess $ isOfType "match Q.D3.D2.D1 => () end" "D1 -> Unit"
                                ]
                          ]
                    , testExpectSuccess
                          "let rec datatype L of LNil; subtype datatype L1 of LCons Unit L end end in pass"
                    , testExpectSuccess
                          "let rec datatype L +a of LNil; subtype datatype L1 of LCons a (L a) end end in pass"
                    ]
              , tGroup
                    "record-constructor"
                    [ tGroup
                          "rank-1"
                          [ tDecls
                                [ "datatype R of Mk of di: Integer end end"
                                , "mkR1: Integer -> R = fn x => Mk.R of di = x end"
                                , "mkR2: Integer -> R = fn di => Mk.R"
                                ] $
                            tGroup
                                "one"
                                [ testExpectSuccess "pass"
                                , testExpectSuccess "testeq {21} {(fn Mk.R => di) (mkR1 21)}"
                                , testExpectSuccess "testeq {21} {(fn Mk.R => di) (mkR2 21)}"
                                , testExpectSuccess "testeq {22} {(match Mk.R => di end) (mkR1 22)}"
                                , testExpectSuccess "testeq {22} {(match Mk.R => di end) (mkR2 22)}"
                                , testExpectSuccess "testeq {23} {let Mk.R = mkR1 23 in 23}"
                                , testExpectSuccess "testeq {23} {let Mk.R = mkR2 23 in 23}"
                                , testExpectSuccess "testeq {24} {let Mk.R = mkR1 24 in di}"
                                , testExpectSuccess "testeq {24} {let Mk.R = mkR2 24 in di}"
                                , testExpectSuccess "testeq {25} {let Just di = Just 25 in di}"
                                , testExpectSuccess "let g: R -> Integer = fn Mk.R => di; in testeq {26} {g $ mkR1 26}"
                                , testExpectReject "testeq {27} {let Mk.R as A = mkR1 27 in di}"
                                , testExpectSuccess "testeq {28} {let Mk.R as A = mkR1 28 in di.A}"
                                ]
                          , tDecls ["datatype R of Mk of di: Integer; dt: Text end end"] $
                            tGroup
                                "two"
                                [ testExpectSuccess "pass"
                                , testExpectSuccess
                                      "let f: Integer -> R = fn di => let dt = \"t\" in Mk.R; g: R -> Integer = fn Mk.R => di; in testeq {17} {g $ f 17}"
                                , testExpectSuccess
                                      "let f: Integer -> R = fn di => let dt = \"t\" in Mk.R; g: R -> Text = fn Mk.R => dt; in testeq {\"t\"} {g $ f 17}"
                                ]
                          ]
                    , tDecls
                          [ "datatype UU of Mk of r1: Unit; r2: Unit; end end"
                          , "testr = fn f1, a1, f2, a2 => let r1 = f1 a1; r2 = f2 a2; in Mk.UU"
                          , "testrs = fn f1, a1, f2, a2 => let r1:Unit = f1 a1; r2:Unit = f2 a2; in Mk.UU"
                          , "testps = fn f1, a1, f2, a2 => let r1:Unit = f1 a1; r2:Unit = f2 a2; in (r1,r2)"
                          ] $
                      tGroup
                          "issue-192"
                          [ testExpectSuccess "pass"
                          , testExpectSuccess $ isOfType "testr" "(a -> Unit) -> a -> (a -> Unit) -> a -> UU"
                          , testExpectSuccess $ isOfType "testr" "(a -> Unit) -> a -> (b -> Unit) -> b -> UU"
                          , testExpectSuccess $ isOfType "testrs" "(a -> Unit) -> a -> (a -> Unit) -> a -> UU"
                          , testExpectSuccess $ isOfType "testrs" "(a -> Unit) -> a -> (b -> Unit) -> b -> UU"
                          , testExpectSuccess $ isOfType "testps" "(a -> Unit) -> a -> (a -> Unit) -> a -> Unit *: Unit"
                          , testExpectSuccess $ isOfType "testps" "(a -> Unit) -> a -> (b -> Unit) -> b -> Unit *: Unit"
                          ]
                    , tDecls
                          [ "datatype R of Mk of df: (a -> a) -> a -> a end end"
                          , "twice = fn f, x => f (f x)"
                          , "addone: Integer -> Integer = fn x => x + 1"
                          ] $
                      tGroup
                          "rank-2"
                          [ testExpectSuccess "pass"
                          , testExpectSuccess
                                "let r:R = Mk.R of df = twice end in r >- fn Mk.R => testeq {9} {df addone 7}"
                          , testExpectSuccess
                                "let r:R = let df: (b -> b) -> b -> b = twice in Mk.R in r >- fn Mk.R => testeq {9} {df addone 7}"
                          , testExpectReject
                                "let r:R = let df: (Integer -> Integer) -> Integer -> Integer = twice in Mk.R in r >- fn Mk.R => testeq {9} {df addone 7}"
                          ]
                    , tDecls
                          [ "datatype R +v of Mk of df: a -> Maybe (a *: v) end end"
                          , "ff: a -> Maybe (a *: Integer) = fn x => Just (x,45)"
                          , "r = Mk.R of df = ff end"
                          ] $
                      tGroup
                          "sub-var"
                          [ testExpectSuccess "pass"
                          , testExpectSuccess $ isOfType "r" "R Integer"
                          , testExpectReject $ isOfType "r" "R Text"
                          ]
                    , tGroup
                          "inversion"
                          [ tDecls
                                [ "datatype W +a of Mk a end"
                                , "wrap: a -> W a = Mk.W"
                                , "unwrap: W a -> a = fn (Mk.W val) => val"
                                ] $
                            tGroup
                                "plain"
                                [testExpectSuccess "pass", testExpectSuccess "testeq {374} {unwrap $ wrap 374}"]
                          , tGroup
                                "record"
                                [ tDecls
                                      [ "datatype W +p of Mk of val: p end end"
                                      , "wrap: x -> W x = fn val => Mk.W"
                                      , "unwrap: W y -> y = fn Mk.W => val"
                                      ] $
                                  tGroup
                                      "id"
                                      [testExpectSuccess "pass", testExpectSuccess "testeq {374} {unwrap $ wrap 374}"]
                                , tDecls
                                      [ "datatype W +p of Mk of val: p end end"
                                      , "wrap: x -> W x = fn v => let val = v in Mk.W"
                                      , "unwrap: W y -> y = fn Mk.W => val"
                                      ] $
                                  tGroup
                                      "let"
                                      [testExpectSuccess "pass", testExpectSuccess "testeq {374} {unwrap $ wrap 374}"]
                                , tDecls
                                      [ "datatype W +p of Mk of val: Maybe p end end"
                                      , "wrap: Maybe x -> W x = fn val => Mk.W"
                                      , "unwrap: W y -> Maybe y = fn Mk.W => val"
                                      ] $
                                  tGroup
                                      "Maybe"
                                      [ testExpectSuccess "pass"
                                      , testExpectSuccess "testeq {Just 374} {unwrap $ wrap $ Just 374}"
                                      ]
                                , tDecls
                                      [ "datatype R +p of Mk of val: q -> (q *: p) end end"
                                      , "f = fn x => let val = fn y => (y,x) in Mk.R"
                                      ] $
                                  tGroup
                                      "infer-var"
                                      [testExpectSuccess "pass", testExpectSuccess $ isOfType "f" "a -> R a"]
                                , tDecls
                                      [ "datatype R +p of Mk of val: q -> (q *: p) end end"
                                      , "f = fn x => let val = fn y => (y,x+1) in Mk.R"
                                      ] $
                                  tGroup
                                      "infer-Integer"
                                      [ testExpectSuccess "pass"
                                      , testExpectSuccess $ isOfType "f" "Integer -> R Integer"
                                      ]
                                ]
                          ]
                    , tDecls
                          [ "datatype R +a of Mk of val: List a end end"
                          , "mkR: List a -> R a = fn val => Mk.R"
                          , "rShow: R Showable -> Text = fn Mk.R => show val"
                          ] $
                      tGroup
                          "map"
                          [ testExpectSuccess "pass"
                          , testExpectSuccess
                                "let r1: R Integer = mkR []; r2: R Showable = r1 in testeq {\"[]\"} {rShow r2}"
                          , testExpectSuccess
                                "let r1: R Integer = mkR [57]; r2: R Showable = r1 in testeq {\"[57]\"} {rShow r2}"
                          , testExpectSuccess
                                "let r1: R Integer = mkR [12, 10, 57]; r2: R Showable = r1 in testeq {\"[12,10,57]\"} {rShow r2}"
                          ]
                    , tDecls
                          [ "datatype Rec +a of Mk of rval: rec r, Maybe (a *: r) end end"
                          , "rec0: Rec a = let rval = Nothing in Mk.Rec"
                          , "rec1: a -> Rec a = fn x0 => let rval = Just (x0,Nothing) in Mk.Rec"
                          , "rec3: a -> a -> a -> Rec a = fn x0, x1, x2 => let rval = Just (x0,Just (x1,Just (x2,Nothing))) in Mk.Rec"
                          , "let rec rShow: (rec r, Maybe (Showable *: r)) -> Text = match Nothing => \"\"; Just (a,r) => show a <>.Text \",\" <>.Text rShow r end end"
                          , "recShow: Rec Showable -> Text = fn Mk.Rec => rShow rval"
                          ] $
                      tGroup
                          "recursive"
                          [ testExpectSuccess "pass"
                          , testExpectSuccess
                                "let r1: Rec Integer = rec0; r2: Rec Showable = r1 in testeq {\"\"} {recShow r2}"
                          , testExpectSuccess
                                "let r1: Rec Integer = rec1 57; r2: Rec Showable = r1 in testeq {\"57,\"} {recShow r2}"
                          , testExpectSuccess
                                "let r1: Rec Integer = rec3 12 10 57; r2: Rec Showable = r1 in testeq {\"12,10,57,\"} {recShow r2}"
                          ]
                    , tGroup
                          "override"
                          [ tGroup "type" $ let
                                testOverride :: Text -> Text -> Text
                                testOverride ta tb =
                                    "let datatype A of Mk of val: " <>
                                    ta <> " end end; datatype B <: A of Mk of Mk.A; val: " <> tb <> " end end in pass"
                                in [ testExpectSuccess $ testOverride "Integer" "Integer"
                                   , testExpectSuccess $ testOverride "Rational" "Integer"
                                   , testExpectReject $ testOverride "Integer" "Rational"
                                   , testExpectReject $ testOverride "Integer *: Rational" "Rational *: Integer"
                                   , testExpectReject $ testOverride "Integer" "Text"
                                   , testExpectSuccess $ testOverride "a -> a" "a -> a"
                                   , testExpectSuccess $ testOverride "a -> a" "b -> b"
                                   , testExpectSuccess $ testOverride "Integer -> Integer" "a -> a"
                                   , testExpectReject $ testOverride "a -> a" "Integer -> Integer"
                                   ]
                          , tDecls
                                [ "datatype A of Mk of val: Integer end end"
                                , "datatype B of Mk of val: Rational end end"
                                ] $
                            tGroup
                                "multiple"
                                [ testExpectSuccess "pass"
                                , testExpectReject "let datatype C <: A & B of Mk of Mk.A; Mk.B end end in pass"
                                , testExpectReject
                                      "let datatype C <: A & B of Mk of Mk.A; Mk.B; val: Rational end end in pass"
                                , testExpectSuccess
                                      "let datatype C <: A & B of Mk of Mk.A; Mk.B; val: Integer end end in pass"
                                ]
                          ]
                    , tDecls ["datatype R of Mk of u: Unit end end"] $ let
                          testExpr t = testExpectSuccess $ "let f = " <> t <> " in seq f pass"
                          in tGroup
                                 "issue-199"
                                 [ testExpectSuccess "pass"
                                 , testExpr "fn Mk.R => u"
                                 , testExpr "fn a => let Mk.R = a in u"
                                 , testExpr "fn Mk.R => let Mk.R = Mk.R in u"
                                 , testExpr "fn Mk.R => fn Mk.R => u"
                                 , testExpr "fn a => let Mk.R = a; Mk.R = a; in u"
                                 ]
                    , tGroup
                          "supertype"
                          [ tDecls
                                [ "datatype S of Mk of sval: Text end end"
                                , "mkS: Text -> S = fn sval => Mk.S"
                                , "unS: S -> Text = fn Mk.S => sval"
                                , "datatype R <: S of Mk of Mk.S; rval: Integer end end"
                                , "mkR: Text *: Integer -> R = fn (sval,rval) => Mk.R"
                                , "unR: R -> Text *: Integer = fn Mk.R => (sval,rval)"
                                ] $
                            tGroup
                                "simple"
                                [ testExpectSuccess "pass"
                                , testExpectSuccess "testeq {(\"textx\", 56)}  {unR $ mkR (\"textx\", 56)}"
                                , subtypeTest False SRSingle "R" "S"
                                , subtypeTest False SRNot "S" "R"
                                ]
                          , tDecls
                                [ "datatype S of Mk of val: Rational end end"
                                , "mkS: Rational -> S = fn val => Mk.S"
                                , "unS: S -> Rational = fn Mk.S => val"
                                , "datatype R <: S of Mk of Mk.S; val: Integer end end"
                                , "mkR: Integer -> R = fn val => Mk.R"
                                , "unR: R -> Integer = fn Mk.R => val"
                                ] $
                            tGroup
                                "refinement"
                                [ testExpectSuccess "pass"
                                , testExpectSuccess "testeq {74}  {unS $ mkR 74}"
                                , subtypeTest False SRSingle "R" "S"
                                , subtypeTest False SRNot "S" "R"
                                ]
                          , tDecls
                                [ "datatype S1 of Mk of val: Rational end end"
                                , "datatype S2 of Mk of val: Rational end end"
                                ] $
                            tGroup
                                "match-type"
                                [ testExpectSuccess "let datatype R <: S1 of Mk of Mk.S1 end end in pass"
                                , testExpectReject "let datatype R <: S1 of Mk of Mk.S2 end end in pass"
                                , testExpectSuccess "let datatype R of Mk of end end in pass"
                                , testExpectReject "let datatype R <: S1 of Mk of end end in pass"
                                , testExpectReject "let datatype R of Mk of Mk.S1 end end in pass"
                                , testExpectSuccess
                                      "let datatype R <: S1 & S2 of Mk of Mk.S1; Mk.S2; val: Rational end end in pass"
                                , testExpectSuccess
                                      "let datatype R <: S1 & S2 of Mk of Mk.S1; Mk.S2; val: Integer end end in pass"
                                , testExpectReject "let datatype R <: S1 & S2 of Mk of Mk.S1 end end in pass"
                                , testExpectReject "let datatype R <: S1 & S2 of Mk of Mk.S2 end end in pass"
                                ]
                          , tGroup
                                "multiple-supertype"
                                [ tDecls
                                      [ "datatype A of Mk of ma: Integer end end"
                                      , "datatype B of Mk of mb: Text end end"
                                      , "datatype C <: A & B of Mk of Mk.A; Mk.B end end"
                                      , "c = let ma = 75; mb = \"ttmb\" in Mk.C"
                                      ] $
                                  tGroup
                                      "single-constructor"
                                      [ testExpectSuccess "pass"
                                      , testExpectSuccess "testeq {(\"ttmb\",75)} {c >- fn Mk.B@Mk.A => (mb,ma)}"
                                      , testExpectSuccess "testeq {(\"ttmb\",75)} {c >- fn Mk.C => (mb,ma)}"
                                      ]
                                , tDecls
                                      [ "datatype A of Mk1 of ma1: Integer end; Mk2 of ma2: Text end; end"
                                      , "datatype B of Mk1 of mb1: Integer end; Mk2 of mb2: Text end; end"
                                      , "datatype C <: A & B of Mk of Mk2.A; Mk1.B end end"
                                      , "c = let mb1 = 77; ma2 = \"ttma1\" in Mk.C"
                                      ] $
                                  tGroup
                                      "multiple-constructor"
                                      [ testExpectSuccess "pass"
                                      , testExpectSuccess "testeq {(\"ttma1\",77)} {c >- fn Mk2.A@Mk1.B => (ma2,mb1)}"
                                      , testExpectSuccess "testeq {(\"ttma1\",77)} {c >- fn Mk.C => (ma2,mb1)}"
                                      ]
                                , tDecls
                                      [ "datatype A of Mk1 of ma1: Integer end; Mk2 of ma2: Text end; end"
                                      , "datatype B <: A of Mk1 of Mk1.A; mb1: Integer end; Mk2 of Mk2.A; mb2: Text end; end"
                                      , "datatype C <: A of Mk1 of Mk1.A; mc1: Integer end; Mk2 of Mk2.A; mc2: Text end; end"
                                      ] $
                                  tGroup
                                      "diamond"
                                      [ testExpectSuccess "pass"
                                      , tDecls
                                            [ "datatype D <: B & C of Mk of Mk1.B; Mk1.C; end end"
                                            , "d = let ma1 = 58; mb1 = 59; mc1 = 60 in Mk.D"
                                            ] $
                                        tGroup
                                            "consistent"
                                            [ testExpectSuccess "pass"
                                            , testExpectSuccess "testeq {(58,59,60)} {d >- fn Mk.D => (ma1,mb1,mc1)}"
                                            , testExpectSuccess
                                                  "testeq {(58,59,60)} {d >- fn Mk1.B@Mk1.C => (ma1,mb1,mc1)}"
                                            ]
                                      , tDecls ["datatype D <: B & C of Mk of Mk1.B; Mk2.C; end end"] $
                                        tGroup "inconsistent" [testExpectReject "pass"]
                                      ]
                                ]
                          ]
                    , tGroup
                          "default"
                          [ tDecls ["datatype A of Mk of ma: Integer = 754 end end"] $
                            tGroup
                                "simple"
                                [ testExpectSuccess "pass"
                                , testExpectSuccess "testeq {42} {Mk.A of ma = 42 end >- fn Mk.A => ma}"
                                , testExpectSuccess "testeq {42} {(let ma = 42 in Mk.A) >- fn Mk.A => ma}"
                                , testExpectSuccess "testeq {754} {Mk.A of end >- fn Mk.A => ma}"
                                , testExpectSuccess "testeq {754} {Mk.A >- fn Mk.A => ma}"
                                ]
                          , tDecls
                                [ "datatype A of Mk1 of ma1: Integer = 755 end; Mk2 of ma2: Text end; end"
                                , "datatype B <: A of Mk1 of Mk1.A; mb1: Integer end; Mk2 of Mk2.A; mb2: Text end; end"
                                , "datatype C <: A of Mk1 of Mk1.A; mc1: Integer end; Mk2 of Mk2.A; mc2: Text end; end"
                                , "datatype D <: B & C of Mk of Mk1.B; Mk1.C; end end"
                                ] $
                            tGroup
                                "diamond"
                                [ testExpectSuccess "pass"
                                , testExpectSuccess
                                      "testeq {43} {Mk.D of ma1 = 43; mb1 = 44; mc1 = 45 end >- fn Mk.D => ma1}"
                                , testExpectSuccess
                                      "testeq {43} {(let ma1 = 43; mb1 = 44; mc1 = 45 in Mk.D) >- fn Mk.D => ma1}"
                                , testExpectSuccess "testeq {755} {Mk.D of mb1 = 46; mc1 = 47 end >- fn Mk.D => ma1}"
                                , testExpectSuccess "testeq {755} {(let mb1 = 46; mc1 = 47 in Mk.D) >- fn Mk.D => ma1}"
                                ]
                          ]
                    ]
              ]
        , tGroup
              "subtype-decl"
              [ tDecls ["datatype T of T1 Integer end", "unT1 = fn T1.T x => x"] $
                tGroup
                    "simple"
                    [ tDecls ["subtype Integer <: T = T1.T"] $
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
              , tDecls ["datatype T +a of T1 (Maybe a) end", "unT1 = fn T1.T x => x"] $
                tGroup
                    "parameter"
                    [ tDecls ["subtype Maybe Integer <: T Integer = T1.T"] $
                      tGroup
                          "plain"
                          [ testExpectSuccess "pass"
                          , subtypeTest False SRSingle "Maybe Integer" "T Integer"
                          , testExpectSuccess "testeq {Just 3} {unT1 $ Just 3}"
                          ]
                    , tDecls ["subtype Maybe a <: T a = T1.T"] $
                      tGroup
                          "tyvar"
                          [ testExpectSuccess "pass"
                          , subtypeTest False SRSingle "Maybe Integer" "T Integer"
                          , testExpectSuccess "testeq {Just 3} {unT1 $ Just 3}"
                          ]
                    ]
              , tDecls ["datatype T +a of T1 (Maybe a) end", "unT1 = fn T1.T x => x"] $
                tGroup
                    "dependent"
                    [ testExpectSuccess $
                      "let x = 17; f = let subtype Unit <: T Integer = fn () => T1.T (Just x) in unT1 () in testeq {Just 17} {f}"
                    , testExpectSuccess $
                      "let rec f = let subtype Unit <: T Integer = fn () => T1.T (Just x) in unT1 (); x = 17 in testeq {Just 17} {f}"
                    , testExpectSuccess $
                      "let f = fn x => let subtype Unit <: T Integer = fn () => T1.T (Just x) in unT1 () in testeq {Just 17} {f 17}"
                    , testExpectSuccess $
                      "let f = fn x => let y = x; subtype Unit <: T Integer = fn () => T1.T (Just y) in unT1 () in testeq {Just 17} {f 17}"
                    ]
              ]
        , tDecls ["datatype storable T of T1 Text Number !\"T.T1\"; T2 !\"T.T2\"; T3 Boolean !\"T.T3\" end"] $
          tGroup
              "datatype-storable"
              [ testExpectSuccess "pass"
              , testExpectSuccess "let f: T -> Entity = fn x => x in pass"
              , testExpectSuccess "let t1 = T1.T \"hello\" 3 in pass"
              , testExpectSuccess "let f = fn T1.T x _ => x in pass"
              , testExpectSuccess "T1.T \"hello\" 3 >- match T1.T \"hello\" 3 => pass end"
              , testExpectSuccess
                    "T1.T \"hello\" 3 >- match T2.T => fail \"T2\"; T1.T \"hello\" 2 => fail \"T1.T 2\"; T1.T \"hell\" 3 => fail \"T1.T hell\"; T1.T \"hello\" 3 => pass end"
              , testExpectSuccess "let datatype storable P of end in pass"
              , testExpectSuccess "let datatype storable P of P1 !\"P1\" end in pass"
              , testExpectSuccess "let datatype storable P of P1 !\"P1\"; end in pass"
              , testExpectSuccess "let datatype storable P of P1 Integer !\"P1\" end in pass"
              , testExpectSuccess "let datatype storable P of P1 Integer !\"P1\"; end in pass"
              , testExpectSuccess "let datatype storable P of P1 Integer !\"P1\"; P2 Text !\"P2\" end in pass"
              , testExpectSuccess "let datatype storable P of P1 Integer !\"P1\"; P2 Text !\"P2\"; end in pass"
              , tGroup
                    "nominal"
                    [ testExpectSuccess "let datatype storable P of P1 !\"P1\" end; f : P -> P = fn x => x in pass"
                    , testExpectReject
                          "let datatype storable P of P1 !\"P1\" end; datatype storable Q of end; f : P -> Q = fn x => x in pass"
                    , testExpectReject
                          "let datatype storable P of end; datatype storable Q of Q1 !\"Q1\" end; f : P -> Q = fn x => x in pass"
                    , testExpectReject
                          "let datatype storable P of end; datatype storable Q of end; f : P -> Q = fn x => x in pass"
                    , testExpectReject
                          "let datatype storable P of P1 !\"P1\" end; datatype storable Q of Q1 !\"Q1\" end; f : P -> Q = fn x => x in pass"
                    , testExpectReject
                          "let datatype storable P of P1 Integer !\"P1\" end; datatype storable Q of Q1 Integer !\"Q1\" end; f : P -> Q = fn x => x in pass"
                    ]
              , tGroup
                    "parameters"
                    [ testExpectSuccess
                          "let datatype storable P +a of P1 !\"P1\" end; f : P Integer -> P Integer = fn x => x in pass"
                    , testExpectReject
                          "let datatype storable P -a of P1 !\"P1\" end; f : P Integer -> P Integer = fn x => x in pass"
                    , testExpectReject
                          "let datatype storable P a of P1 !\"P1\" end; f : P Integer -> P Integer = fn x => x in pass"
                    , testExpectSuccess
                          "let datatype storable P +a of P1 a !\"P1\" end; f : P Integer -> P Integer = fn x => x in pass"
                    , testExpectSuccess
                          "let datatype storable P +a of P1 a !\"P1\" end; f : P Integer -> Integer= fn P1.P x => x in pass"
                    , testExpectSuccess "let datatype storable P of P1 !\"P1\" end; f : P -> Entity = fn x => x in pass"
                    , testExpectSuccess
                          "let datatype storable P +a of P1 a !\"P1\" end; f : P Entity -> Entity = fn x => x in pass"
                    , testExpectSuccess
                          "let datatype storable P +a +b of P1 a b !\"P1\" end; f : P Entity Entity -> Entity = fn x => x in pass"
                    ]
              , tGroup
                    "recursive"
                    [ testExpectSuccess
                          "let datatype storable P of P1 !\"P1\" end in let datatype storable Q of Q1 P !\"Q1\" end in pass"
                    , testExpectSuccess
                          "let datatype storable P of P1 !\"P1\" end; datatype storable Q of Q1 P !\"Q1\" end in pass"
                    , testExpectSuccess
                          "let rec datatype storable P of P1 !\"P1\" end; datatype storable Q of Q1 P !\"Q1\" end in pass"
                    , testExpectSuccess
                          "let rec datatype storable P of P1 Q !\"P1\" end; datatype storable Q of end in pass"
                    , testExpectSuccess
                          "let rec datatype storable P of P1 Q !\"P1\" end; datatype storable Q of Q1 P !\"Q1\" end in pass"
                    , testExpectSuccess "let rec datatype storable P of P1 P !\"P1\" end in pass"
                    , testExpectSuccess "let rec datatype storable P +a of P1 (P (a *: a)) !\"P1\" end in pass"
                    , tDecls
                          [ "let rec datatype storable Nat of Z !\"Z\"; S Nat !\"S\" end end"
                          , "let rec natToInteger: Nat -> Integer = match Z.Nat => 0; S.Nat n => 1 + natToInteger n end end"
                          ] $
                      tGroup
                          "Nat"
                          [ testExpectSuccess "pass"
                          , testExpectSuccess "testeq {0} {natToInteger Z.Nat}"
                          , testExpectSuccess "testeq {1} {natToInteger $ S.Nat Z.Nat}"
                          , testExpectSuccess "testneq {0} {Z.Nat}"
                          , testExpectSuccess "testeq {Z.Nat} {Z.Nat}"
                          ]
                    , tDecls
                          [ "let rec datatype storable L +a of Nil !\"Nil\"; Cons a (L a) !\"Cons\" end end"
                          , "let rec listToL: List a -> L a = match [] => Nil.L; x::xs => Cons.L x (listToL xs) end end"
                          , "let rec lToList: L a -> List a = match Nil.L => []; Cons.L x xs => x :: lToList xs end end"
                          ] $
                      tGroup
                          "list"
                          [ testExpectSuccess "pass"
                          , testExpectSuccess "let l = listToL [1,2,3] in pass"
                          , testExpectSuccess "let l = listToL [1,2,3] in testeq {lToList l} {[1,2,3]}"
                          , testExpectSuccess "testeq {lToList $ listToL [1,2,3]} {[1,2,3]}"
                          , testExpectSuccess "testneq {0} {Nil.L}"
                          , testExpectSuccess "testeq {Nil.L} {Nil.L}"
                          , testExpectSuccess "testeq {Cons.L 1 Nil.L} {Cons.L 1 Nil.L}"
                          , testExpectSuccess "testeq {Cons.L 1 (Cons.L 2 (Cons.L 3 Nil.L))} {Cons.L 1 (Cons.L 2 (Cons.L 3 Nil.L))}"
                          , testExpectSuccess "testeq {listToL [1,2,3]} {Cons.L 1 (Cons.L 2 (Cons.L 3 Nil.L))}"
                          , testExpectSuccess "testeq {lToList $ Cons.L 1 $ Cons.L 2 $ Cons.L 3 Nil.L} {[1,2,3]}"
                          ]
                    ]
              ]
        , tGroup
              "type escape"
              [ testExpectSuccess
                    "let opentype T; t = let in point.OpenEntity @T !\"t\"; f = let f : T -> Action Unit = fn _ => pass in f; in f t"
              , testExpectReject
                    "let opentype T1; opentype T2; t = let in point.OpenEntity @T1 !\"t\"; f = let f : T2 -> Action Unit = fn _ => pass in f; in f t"
              , testExpectReject
                    "let t = let opentype T in point.OpenEntity @T !\"t\"; f = let opentype T; f : T -> Action Unit = fn _ => pass in f; in f t"
              , testExpectReject
                    "let t = let opentype T1 in point.OpenEntity @T1 !\"t\"; f = let opentype T2; f : T2 -> Action Unit = fn _ => pass in f; in f t"
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
              , tDecls ["datatype P of Mk Number end"] $
                tGroup
                    "datatype"
                    [ testExpectSuccess "let subtype Number <: P = Mk.P in pass"
                    , testExpectReject "let subtype Number <: P = Mk.P; subtype Number <: P = Mk.P in pass"
                    , testExpectReject "let subtype Number <: P = Mk.P; subtype Integer <: P = Mk.P in pass"
                    ]
              , tDecls
                    [ "datatype A of Mk Number end"
                    , "datatype B of Mk Number end"
                    , "datatype C of Mk Number end"
                    , "datatype D of Mk Number end"
                    , "subtype A <: B = fn Mk.A x => Mk.B x"
                    , "subtype C <: D = fn Mk.C x => Mk.D x"
                    , "subtype A <: D = fn Mk.A x => Mk.D x"
                    ] $
                tGroup
                    "verify"
                    [ testExpectSuccess "pass"
                    , testExpectReject "let subtype B <: B = fn Mk.B x => Mk.B x in pass"
                    , testExpectReject "let subtype B <: C = fn Mk.B x => Mk.C x in pass"
                    ]
              , tDecls ["datatype A of Mk Number end", "datatype B of Mk Number end"] $
                tGroup
                    "trustme"
                    [ testExpectSuccess "pass"
                    , testExpectSuccess "let subtype A <: B = fn Mk.A x => Mk.B x in pass"
                    , testExpectReject
                          "let subtype A <: B = fn Mk.A x => Mk.B x; subtype A <: B = fn Mk.A x => Mk.B x in pass"
                    , testExpectSuccess
                          "let subtype A <: B = fn Mk.A x => Mk.B x; subtype trustme A <: B = fn Mk.A x => Mk.B x in pass"
                    , testExpectSuccess
                          "let subtype trustme A <: B = fn Mk.A x => Mk.B x; subtype A <: B = fn Mk.A x => Mk.B x in pass"
                    ]
              , tDecls ["datatype A +x of Mk x end", "datatype B +x of Mk x end", "datatype C of Mk end"] $
                tGroup
                    "preferred"
                    [ testExpectSuccess "pass"
                    , testExpectSuccess "let subtype trustme A Number <: B Number = fn Mk.A x => Mk.B x in pass"
                    , tModify (testTreeOne "1") $
                      tDecls
                          [ "subtype trustme A Number <: B Number = fn Mk.A x => Mk.B x"
                          , "subtype trustme B Number <: C = fn Mk.B _ => Mk.C"
                          , "subtype trustme A Any <: C = fn Mk.A _ => Mk.C"
                          ] $
                      subtypeTest False SRSingle "A Unit" "C"
                    , tModify (testTreeOne "2") $
                      tDecls
                          [ "subtype trustme A Any <: C = fn Mk.A _ => Mk.C"
                          , "subtype trustme A Number <: B Number = fn Mk.A x => Mk.B x"
                          , "subtype trustme B Number <: C = fn Mk.B _ => Mk.C"
                          ] $
                      subtypeTest False SRSingle "A Unit" "C"
                    , tModify (testTreeOne "3") $
                      tDecls
                          [ "subtype trustme A a <: B a = fn Mk.A x => Mk.B x"
                          , "subtype trustme B Any <: C = fn Mk.B _ => Mk.C"
                          , "subtype trustme A Number <: C = fn Mk.A _ => Mk.C"
                          ] $
                      subtypeTest False SRSingle "A Unit" "C"
                    , tModify (testTreeOne "4") $
                      tDecls
                          [ "subtype trustme A Number <: C = fn Mk.A _ => Mk.C"
                          , "subtype trustme A a <: B a = fn Mk.A x => Mk.B x"
                          , "subtype trustme B Any <: C = fn Mk.B _ => Mk.C"
                          ] $
                      subtypeTest False SRSingle "A Unit" "C"
                    , tGroup
                          "order"
                          [ tGroup
                                "var"
                                [ tDecls
                                      [ "subtype trustme A a <: B a = fn Mk.A x => Mk.B x"
                                      , "subtype trustme A Number <: B Number = fn Mk.A x => Mk.B x"
                                      ] $
                                  subtypeTest False SRSingle "A Unit" "B Unit"
                                , tDecls
                                      [ "subtype trustme A Number <: B Number = fn Mk.A x => Mk.B x"
                                      , "subtype trustme A a <: B a = fn Mk.A x => Mk.B x"
                                      ] $
                                  subtypeTest False SRSingle "A Unit" "B Unit"
                                ]
                          , tGroup
                                "incoherent"
                                [ tDecls
                                      [ "subtype trustme A Integer <: B Integer = fn Mk.A x => Mk.B x"
                                      , "subtype trustme A Number <: B Number = fn Mk.A x => Mk.B x"
                                      ] $
                                  tGroup
                                      "1"
                                      [ subtypeTest False SRNot "A Number" "B Number"
                                      , subtypeTest False SRNot "A Integer" "B Integer"
                                      ]
                                , tDecls
                                      [ "subtype trustme A Number <: B Number = fn Mk.A x => Mk.B x"
                                      , "subtype trustme A Integer <: B Integer = fn Mk.A x => Mk.B x"
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
                                      [ "subtype trustme A Number <: B Number = fn Mk.A x => Mk.B x"
                                      , "subtype trustme A Number <: B Number = fn Mk.A x => Mk.B x"
                                      ] $
                                  subtypeTest False SRSingle "A Number" "B Number"
                                ]
                          , tGroup
                                "sub"
                                [ tDecls
                                      [ "subtype trustme A Integer <: B Number = fn Mk.A x => Mk.B x"
                                      , "subtype trustme A Number <: B Integer = fn Mk.A _ => Mk.B 0"
                                      ] $
                                  subtypeTest False SRSingle "A Number" "B Integer"
                                , tDecls
                                      [ "subtype trustme A Number <: B Integer = fn Mk.A _ => Mk.B 0"
                                      , "subtype trustme A Integer <: B Number = fn Mk.A x => Mk.B x"
                                      ] $
                                  subtypeTest False SRSingle "A Number" "B Integer"
                                ]
                          , tGroup
                                "in"
                                [ tDecls
                                      [ "subtype trustme A Integer <: C = fn Mk.A _ => Mk.C"
                                      , "subtype trustme A Number <: C = fn Mk.A _ => Mk.C"
                                      ] $
                                  subtypeTest False SRSingle "A Number" "C"
                                , tDecls
                                      [ "subtype trustme A Number <: C = fn Mk.A _ => Mk.C"
                                      , "subtype trustme A Integer <: C = fn Mk.A _ => Mk.C"
                                      ] $
                                  subtypeTest False SRSingle "A Number" "C"
                                ]
                          , tGroup
                                "out"
                                [ tDecls
                                      [ "subtype trustme C <: B Integer = fn Mk.C => Mk.B 0"
                                      , "subtype trustme C <: B Number = fn Mk.C => Mk.B 0"
                                      ] $
                                  subtypeTest False SRSingle "C" "B Integer"
                                , tDecls
                                      [ "subtype trustme C <: B Number = fn Mk.C => Mk.B 0"
                                      , "subtype trustme C <: B Integer = fn Mk.C => Mk.B 0"
                                      ] $
                                  subtypeTest False SRSingle "C" "B Integer"
                                ]
                          ]
                    , tDecls ["subtype List (A a) <: A (List a) = fn aa => Mk.A $ map.List (fn Mk.A a => a) aa"] $
                      tGroup
                          "monoid"
                          [ testExpectSuccess "pass"
                          , subtypeTest False SRSingle "List (A Integer)" "A (List Integer)"
                          , subtypeTest True SRSubsume "List (A None)" "A (List None)"
                          ]
                    ]
              ]
        , testOpenUHStore $
          tWith ["Store", "UndoHandler"] $
          tDecls
              [ "opentype E"
              , "eta = property @E @Text !\"eta\" store"
              , "e1 = point.OpenEntity @E !\"e1\""
              , "rt1 = eta !$ {e1}"
              ] $
          tGroup
              "undo"
              [ testExpectSuccess "do rt1 := \"A\"; testeq {\"A\"} rt1; rt1 := \"B\"; testeq {\"B\"} rt1; end"
              , testExpectSuccess
                    "do rt1 := \"A\"; testeq {\"A\"} rt1; rt1 := \"B\"; testeq {\"B\"} rt1; queueUndo undoHandler; testeq {\"A\"} rt1; end"
              , testExpectSuccess
                    "do rt1 := \"A\"; testeq {\"A\"} rt1; rt1 := \"B\"; testeq {\"B\"} rt1; queueUndo undoHandler; testeq {\"A\"} rt1; queueRedo undoHandler; testeq {\"B\"} rt1; end"
              ]
        , tGroup
              "interpret"
              [ testExpectSuccess "do r <- newMem.WholeModel; asText.Integer !$ r := \"37\"; testeq {37} r; end"
              , testExpectSuccess
                    "do r <- newMem.WholeModel; asText.Date !$ r := \"2015-08-12\"; testeq {YearMonthDay 2015 08 12} r; end"
              ]
        , tDecls
              [ "runresult = fn ar, arg => ar >- match Failure err => fail err; Success f => f arg end"
              , "testaction = fn expected, action => do found <- action; testeqval expected found end"
              , "testFailure = fn action => do found <- action; found >- match Failure _ => pass; Success _ => fail \"not Failure\" end end"
              ] $
          tWith ["Eval"] $
          tGroup
              "evaluate"
              [ testExpectSuccess "testaction (Success True) $ evaluate @Boolean \"True\""
              , testExpectSuccess "testaction (Success 5) $ evaluate @Integer \"5\""
              , testExpectSuccess "testaction (Success 5) $ evaluate @Integer \"let x = 5 in x\""
              , testExpectSuccess
                    "do ar <- evaluate @(Integer -> Integer) \"fn x => x +.Integer 1\"; ar >- match Failure err => fail err; Success f => testeqval 8 $ f 7 end end"
              , testExpectSuccess
                    "testaction (Failure \"<evaluate>:1:1: syntax: expecting: expression\") $ evaluate @Integer \"\""
              , testExpectSuccess "testaction (Failure \"<evaluate>:1:1: undefined: f: a\") $ evaluate @Integer \"f\""
              , testExpectSuccess "testFailure $ evaluate @Integer \"\\\"hello\\\"\""
              , testExpectSuccess
                    "do r <- newMem.WholeModel; ar <- evaluate @(WholeModel Integer -> Action Unit) \"fn r => r :=.WholeModel 45\"; runresult ar r; a <- get r; testeqval 45 a; end"
              , testExpectSuccess "testaction 569 $ evaluate @(a -> a) \"fn x => x\" >>= fn Success f => pure $ f 569"
              , testExpectSuccess
                    "testaction 570 $  evaluate @(Integer -> Integer) \"fn x => x\" >>= fn Success f => pure $ f 570"
              ]
        , tGroup
              "text-sort"
              [ testExpectSuccess "testeq {EQ} {order.Text \"a\" \"a\"}"
              , testExpectSuccess "testeq {EQ} {order.Text \"A\" \"A\"}"
              , testExpectSuccess "testeq {LT} {order.Text \"a\" \"A\"}"
              , testExpectSuccess "testeq {LT} {order.Text \"a\" \"b\"}"
              , testExpectSuccess "testeq {LT} {order.Text \"A\" \"b\"}"
              , testExpectSuccess "testeq {LT} {order.Text \"a\" \"B\"}"
              ]
        , tGroup
              "applicative-notation"
              [ testExpectSuccess "testeq {Just 3} {{.Maybe 3}}"
              , testExpectSuccess "testeq {[10,13,11,14]} {{.List %([3,4]) +.Integer %([7,10])}}"
              ]
        , tGroup
              "do-notation"
              [ testExpectSuccess "testeq {Just 3} {do.Maybe pure 3 end}"
              , testExpectSuccess "testeq {[10,13,11,14]} {do.List a <- [3,4]; b <- [7,10]; pure $ a +.Integer b end}"
              ]
        , tGroup
              "task"
              [ testExpectSuccess
                    "do t <- async.Task $ do sleep $ Seconds 0.01; pure True end; v <- wait.Task t; if v then pass else fail \"\" end"
              , testExpectSuccess
                    "do r <- newMem.WholeModel; r := 0; t <- async.Task $ do sleep $ Seconds 0.01; r := 1; end; wait.Task t; v <- get r; if v == 1 then pass else fail \"\" end"
              , testExpectSuccess
                    "do r <- newMem.WholeModel; r := 0; t <- async.Task $ do sleep $ Seconds 0.05; r := 1; end; v <- get r; if v == 0 then pass else fail \"\" end"
              , testExpectSuccess
                    "do r <- newMem.WholeModel; r := 0; t <- run.Lifecycle $ async.Task $ do sleep $ Seconds 0.05; r := 1; end; v <- get r; if v == 1 then pass else fail \"\" end"
              ]
        ]
