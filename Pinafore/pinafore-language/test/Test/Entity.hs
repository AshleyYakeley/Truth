-- stack test pinafore --test-arguments "--pattern entity.current"

module Test.Entity
    ( testEntity
    , testUpdates
    )
where

import Changes.Core
import Shapes

import Pinafore.Test.Internal
import Test.RunScript

testUpdate :: Text -> ScriptTestTree
testUpdate text =
    testExpression text text $ \interpret -> do
        action <- interpret
        (sendUpdate, model) <- testerLiftAction action
        testerLiftView
            $ runEditor (unWModel $ immutableModelToRejectingModel model)
            $ checkUpdateEditor (Known (1 :: Integer))
            $ unliftActionOrFail sendUpdate

testUpdates :: TestTree
testUpdates =
    runScriptTestTree
        $ tGroup "update" [testUpdate "do {model <- newMem.WholeModel; pure.Action (model :=.WholeModel 1, model)}"]

data SubtypeResult
    = SRNot
    | SRUnify
    | SRSubsume
    | SRSingle

isOfType :: Text -> Text -> Text
isOfType v t = "let {x: " <> t <> " = " <> v <> "} pass"

subtypeTests :: Bool -> SubtypeResult -> Text -> Text -> [ScriptTestTree]
subtypeTests polar sr p q =
    [ testExpectSuccess "pass"
    , tGroup
        "unify"
        [ testSubtypeUnify polar sr
            $ "let {f: ("
            <> q
            <> ") -> Unit = fn _ => (); x: "
            <> p
            <> " = undefined; fx = f x} pass"
        , testSubtypeUnify polar sr $ "let rec {f: (" <> q <> ") -> Unit = f; x: " <> p <> " = x; fx = f x} pass"
        ]
    , tGroup "subsume"
        $ [ testSubtypeSubsume polar sr $ "let rec {x: " <> p <> " = x; y: " <> q <> " = x} pass"
          , testSubtypeSubsume polar sr $ "let {x: " <> p <> " = undefined; y: " <> q <> " = x} pass"
          , testSubtypeSubsume polar sr $ "let {x: " <> q <> " = undefined: " <> p <> "} pass"
          , testSubtypeSubsume polar sr $ "let {x = (undefined: " <> p <> "): " <> q <> "} pass"
          , testSubtypeSingle polar sr $ "let {f: (" <> p <> ") -> (" <> q <> ") = fn x => x} pass"
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
    runScriptTestTree
        $ tDecls
            [ "pass = pure ()"
            , "undefined = error \"undefined\""
            , "testeq = fn expected, found => if expected == found then pass else fail \"not equal\""
            , "testneq = fn expected, found => if expected /= found then pass else fail \"equal\""
            , "runWholeModel = fn r => do {a <- get r; a}"
            , "runreforfail = fn r => runWholeModel (r ?? ap{fail \"unknown model\"})"
            , "testrefeq = fn expected, found => runreforfail ap{testeq %expected %found}"
            , "testrefneq = fn expected, found => runreforfail ap{testneq %expected %found}"
            , "testrefisknown = fn t => runWholeModel ap{if %(known t) then pass else fail \"known\"}"
            , "testrefisunknown = fn t => runWholeModel ap{if %(known t) then fail \"known\" else pass}"
            , "expectStop = fn p => onStop (p >> fail \"no stop\") pass"
            ]
        $ tGroup
            "entity"
            [ tDecls []
                $ tGroup
                    "current"
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
                , testExpectReject "pass}"
                , testExpectReject "pass ("
                , testExpectReject "pass )"
                , testExpectReject "pass let"
                ]
            , tGroup
                "fail"
                [ testExpectStop "stop"
                , testExpectThrow "fail \"text\""
                , testExpectThrow "let {} fail \"text\""
                , testExpectThrow "let {t = 1} fail \"text\""
                , testExpectThrow "let {entitytype T} fail \"text\""
                ]
            , tGroup
                "do"
                [ testExpectSuccess "do {pure ()}"
                , testExpectSuccess "do {pure ();}"
                , testExpectSuccess "do {testeq 3 3}"
                , testExpectSuccess "do {a <- pure 3; testeq 3 a}"
                , testExpectSuccess "do {a = 3; testeq 3 a}"
                , testExpectSuccess "do {{# comment #} a = 3; testeq 3 a}"
                , testExpectSuccess "do {{#| doc a #} a = 3; testeq 3 a}"
                , testExpectSuccess "do {a <- pure 3; b <- pure $ a +.Integer a; testeq 6 b}"
                ]
            , tDecls ["flagRef = do {r <- newMem.WholeModel; r := False; pure r;}"]
                $ tGroup
                    "stop"
                    [ testExpectSuccess "pure ()"
                    , testExpectThrow "fail \"failure\""
                    , testExpectSuccess "expectStop stop"
                    , testExpectSuccess "expectStop $ do {stop; fail \"unstopped\";}"
                    , testExpectSuccess "do {a <- onStop (pure 1) (pure 2); testeq 1 a;}"
                    , testExpectSuccess "do {a <- onStop (pure 1) stop; testeq 1 a;}"
                    , testExpectThrow "do {a <- onStop (pure 1) stop; fail \"unstopped\";}"
                    , testExpectSuccess "do {a <- onStop stop (pure 2); testeq 2 a;}"
                    , testExpectThrow "do {a <- onStop stop (pure 2); fail \"unstopped\";}"
                    , testExpectSuccess
                        "do {r1 <- flagRef; r2 <- flagRef; onStop (r1 := True) (r2 := True); testrefeq ap{True} r1; testrefeq ap{False} r2;}"
                    , testExpectSuccess
                        "do {r1 <- flagRef; r2 <- flagRef; onStop (do {r1 := True; stop;}) (r2 := True); testrefeq ap{True} r1; testrefeq ap{True} r2;}"
                    , testExpectSuccess
                        "do {r1 <- flagRef; r2 <- flagRef; onStop (do {stop; r1 := True;}) (r2 := True); testrefeq ap{False} r1; testrefeq ap{True} r2;}"
                    ]
            , tGroup
                "equality"
                [ testExpectSuccess "testeq 1 1"
                , testExpectSuccess "testeq False $ 1 == \"1\""
                , testExpectSuccess "testeq False $ 0 == 1"
                , testExpectSuccess "testeq True $ 1 == 1"
                , testExpectSuccess "testeq False $ 1 == ~1"
                ]
            , tGroup
                "model"
                [ tGroup
                    "notation"
                    [ testExpectSuccess "runreforfail ap{pass}"
                    , testExpectSuccess "let {p = pass} runreforfail ap{p}"
                    , testExpectSuccess "runreforfail ap{let {p = pass} p}"
                    , testExpectSuccess "runreforfail ap{%ap{pass}}"
                    , testExpectSuccess "let {rp = ap{pass}} runreforfail ap{%rp}"
                    , testExpectSuccess "runreforfail ap{let {rp = ap{pass}} %rp}"
                    , testExpectSuccess "let {rp = ap{pass}} runreforfail ap{let {p= %rp} p}"
                    ]
                , tGroup
                    "stop"
                    [ testExpectSuccess "expectStop $ stop"
                    , testExpectSuccess "expectStop $ get unknown"
                    , testExpectSuccess "expectStop $ ap{1} := 1"
                    , testExpectSuccess "expectStop $ delete ap{1}"
                    ]
                , tGroup
                    "memory"
                    [ testExpectSuccess "expectStop $ do {r <- newMem.WholeModel; get r;}"
                    , testExpectSuccess "do {r <- newMem.WholeModel; r := 45; a <- get r; testeq 45 a;}"
                    , testExpectSuccess "do {r <- newMem.WholeModel; r := 3; r := 4; a <- get r; testeq 4 a;}"
                    , testExpectSuccess
                        "do {s <- newMem.FiniteSetModel; n <- get $ count.FiniteSetModel s; testeq 0 n;}"
                    , testExpectSuccess
                        "do {s <- newMem.FiniteSetModel; s +=.SetModel 57; n <- get $ count.FiniteSetModel s; testeq 1 n;}"
                    , testExpectSuccess
                        "do {s <- newMem.FiniteSetModel; s -=.SetModel 57; n <- get $ count.FiniteSetModel s; testeq 0 n;}"
                    , testExpectSuccess
                        "do {s <- newMem.FiniteSetModel; s +=.SetModel 57; s -=.SetModel 57; n <- get $ count.FiniteSetModel s; testeq 0 n;}"
                    , testExpectSuccess
                        "do {s <- newMem.FiniteSetModel; s +=.SetModel 57; m54 <- get $ member.SetModel s ap{54}; m57 <- get $ member.SetModel s ap{57}; testeq False m54; testeq True m57;}"
                    , testExpectSuccess
                        "do {s <- newMem.FiniteSetModel; s -=.SetModel 57; m57 <- get $ member.SetModel s ap{57}; testeq False m57;}"
                    , testExpectSuccess
                        "do {s <- newMem.FiniteSetModel; s +=.SetModel 57; s -=.SetModel 57; m57 <- get $ member.SetModel s ap{57}; testeq False m57;}"
                    , testExpectSuccess
                        "do {s <- newMem.FiniteSetModel; member.SetModel s ap{57} := True; m54 <- get $ member.SetModel s ap{54}; m57 <- get $ member.SetModel s ap{57}; testeq False m54; testeq True m57;}"
                    , testExpectSuccess
                        "do {s <- newMem.FiniteSetModel; member.SetModel s ap{57} := False; m57 <- get $ member.SetModel s ap{57}; testeq False m57;}"
                    , testExpectSuccess
                        "do {s <- newMem.FiniteSetModel; member.SetModel s ap{57} := True; member.SetModel s ap{57} := False; m57 <- get $ member.SetModel s ap{57}; testeq False m57;}"
                    , testExpectSuccess "expectStop $ do {r <- newMem.WholeModel; immut.WholeModel r := 5;}"
                    ]
                , tDecls
                    [ "showVal: Showable -> Action Unit = fn v => message.Debug $ show v"
                    , "showList: List Showable -> Action Unit = fn l => do {message.Debug \"[[[\"; for_ l showVal;  message.Debug \"]]]\";}"
                    , "testImmutList = fn present, n, call => do {lr <- newMem.ListModel; lr := [10,20,30]; r <- item.ListModel present n lr; ir <- item.ListModel present n $ immut.ListModel lr; call lr; a <- get r; ia <- get ir; testeq a ia;}"
                    ]
                    $ tGroup
                        "list"
                        [ testExpectSuccess "pass"
                        , testExpectSuccess "do {r <- newMem.ListModel; n <- getCount.ListModel r; testeq 0 n;}"
                        , testExpectSuccess
                            "do {r <- newMem.ListModel; r := [10,20,30]; n <- getCount.ListModel r; testeq 3 n;}"
                        , testExpectSuccess "do {r <- newMem.ListModel; n <- get $ count.ListModel r; testeq 0 n;}"
                        , testExpectSuccess
                            "do {r <- newMem.ListModel; r := [10,20,30]; n <- get $ count.ListModel r; testeq 3 n;}"
                        , testExpectSuccess
                            "do {r <- newMem.ListModel; r := [10,20,30]; ir <- item.ListModel True 1 r; i <- get ir; testeq 20 i;}"
                        , testExpectSuccess
                            "do {r <- newMem.ListModel; r := [10,20,30]; ir <- item.ListModel True 1 r; ir := 25; i <- get ir; testeq 25 i;}"
                        , testExpectSuccess
                            "do {r <- newMem.ListModel; r := [10,20,30]; ir <- item.ListModel True 1 r; ir := 25; l <- get r; testeq [10,25,30] l;}"
                        , testExpectSuccess
                            "do {r <- newMem.ListModel; r := [10,20,30]; ir <- item.ListModel True 1 r; delete ir; l <- get r; testeq [10,30] l;}"
                        , testExpectSuccess
                            "do {r <- newMem.ListModel; r := [10,20,30]; ir <- item.ListModel True 1 r; delete ir; ir := 15; l <- get r; testeq [10,15,30] l;}"
                        , testExpectSuccess
                            "do {r <- newMem.ListModel; r := [10,20,30]; ir <- item.ListModel False 1 r; i <- expectStop $ get ir; pure ();}"
                        , testExpectSuccess
                            "do {r <- newMem.ListModel; r := [10,20,30]; ir <- item.ListModel False 1 r; ir := 25; i <- get ir; testeq 25 i;}"
                        , testExpectSuccess
                            "do {r <- newMem.ListModel; r := [10,20,30]; ir <- item.ListModel False 1 r; ir := 25; l <- get r; testeq [10,25,20,30] l;}"
                        , testExpectSuccess
                            "do {r <- newMem.ListModel; r := [10,20,30]; ir <- item.ListModel False 1 r; delete ir; l <- get r; testeq [10,20,30] l;}"
                        , testExpectSuccess
                            "do {r <- newMem.ListModel; r := [10,20,30]; ir <- item.ListModel False 1 r; delete ir; ir := 15; l <- get r; testeq [10,15,20,30] l;}"
                        , testExpectSuccess
                            "do {r <- newMem.ListModel; r := [10,20,30]; ir <- item.ListModel False 1 r; delete ir; l <- get r; testeq [10,20,30] l;}"
                        , testExpectSuccess
                            "do {r <- newMem.ListModel; r := [10,20,30]; ir <- item.ListModel False 1 r; delete ir; ir := 15; l <- get r; testeq [10,15,20,30] l;}"
                        , testExpectSuccess
                            "do {r <- newMem.ListModel; r := [10,20,30]; ir <- item.ListModel True 1 r; insert.ListModel 1 12 r; i <- get ir; testeq 20 i;}"
                        , testExpectSuccess
                            "do {r <- newMem.ListModel; r := [10,20,30]; ir <- item.ListModel True 1 r; insert.ListModel 1 12 r; ir := 15; l <- get r; testeq [10,12,15,30] l;}"
                        , testExpectSuccess "testImmutList True 1 $ fn _ => pure ()"
                        ]
                ]
            , tDecls
                [ "convr : Rational -> Rational= id"
                , "convn : Number -> Number = id"
                , "convl : Literal -> Literal = id"
                , "testconvr : Rational -> Action Unit = fn r => testeq (convl r) (convl $ convn r)"
                ]
                $ tGroup
                    "literal"
                    [ tGroup
                        "Rational to Number"
                        [ testExpectSuccess "testconvr 1"
                        , testExpectSuccess "testconvr 2.5"
                        , testExpectSuccess "testeq (convl 31.5) (convl $ convn 31.5)"
                        , testExpectSuccess "testeq \"63/2\" (show 31.5)"
                        , testExpectSuccess "testeq \"63/2\" (show $ convn 31.5)"
                        ]
                    , let
                        testLiteralConversion :: ScriptExpectation -> Text -> Text -> ScriptTestTree
                        testLiteralConversion se ptype val =
                            testScriptExpectation (val <> ": " <> ptype <> " => " <> pack (show se)) se
                                $ "(("
                                <> val
                                <> "): Literal) >- fn {val:? "
                                <> ptype
                                <> " => testeq val ("
                                <> val
                                <> "); _ => stop}"
                        testPairs :: [(Text, Text)]
                        testPairs =
                            [ ("Boolean", "True")
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
                            ,
                                ( "Time"
                                , "toTime.LocalTime -480 $ DateAndTime (YearMonthDay 2022 01 16) (HourMinuteSecond 19 07 22)"
                                )
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
                        [ testExpectSuccess "testeq \"False\" $ show False"
                        , testExpectSuccess "testeq \"34\" $ show 34"
                        , testExpectSuccess "testeq \"\\\"hello\\\"\" $ show \"hello\""
                        ]
                    ]
            , tOpenDefaultStore
                $ tWith ["Store"]
                $ tDecls
                    [ "entitytype E"
                    , "eea = !{property @E @E !\"eea\"} store"
                    , "eeb = !{property @E @E !\"eeb\"} store"
                    , "eec = !{property @E @E !\"eec\"} store"
                    , "eed = !{property @E @E !\"eed\"} store"
                    , "eta = !{property @E @Text !\"eta\"} store"
                    , "eia = !{property @E @Integer !\"eia\"} store"
                    , "eib = !{property @E @Integer !\"eib\"} store"
                    , "eic = !{property @E @Integer !\"eic\"} store"
                    , "tea = !{property @Text @E !\"tea\"} store"
                    , "nea = !{property @Integer @E !\"nea\"} store"
                    , "e1 = !{point.OpenEntity @E !\"e1\"}"
                    , "e2 = !{point.OpenEntity @E !\"e2\"}"
                    , "e3 = !{point.OpenEntity @E !\"e3\"}"
                    , "e4 = !{point.OpenEntity @E !\"e4\"}"
                    , "eba = !{property @E @Boolean !\"eba\"} store"
                    , "era = !{property @E @Rational !\"era\"} store"
                    , "ena = !{property @E @Number !\"ena\"} store"
                    ]
                $ tGroup
                    "storage"
                    [ testExpectSuccess "pass"
                    , tGroup
                        "unknown & known"
                        [ testExpectSuccess "testrefisunknown ap{% (eta !$ ap{e1}) == % (eta !$ ap{e1})}"
                        , testExpectSuccess "runreforfail ap{if %(known unknown) then fail \"failed\" else pass}"
                        , testExpectSuccess "runreforfail ap{if %(known $ eta !$ ap{e1}) then fail \"failed\" else pass}"
                        , testExpectSuccess
                            "pass >> runreforfail ap{if %(known $ eta !$ ap{e1}) then fail \"failed\" else pass}"
                        , testExpectSuccess
                            "runreforfail ap{pass >>.Action if %(known $ eta !$ ap{e1}) then fail \"failed\" else pass}"
                        , testExpectSuccess
                            "runreforfail ap{if %(known $ eta !$ ap{e1}) then fail \"failed\" else pass} >> pass"
                        , testExpectSuccess "testrefisunknown unknown"
                        , testExpectSuccess "testrefisunknown (eta !$ ap{e1})"
                        , testExpectSuccess "testrefisunknown $ unknown ?? unknown"
                        , testExpectSuccess "testrefeq ap{0} $ unknown ?? ap{0}"
                        , testExpectSuccess "testrefeq ap{1} $ ap{1} ?? ap{0}"
                        , testExpectSuccess "testrefeq ap{1} $ ap{1} ?? unknown"
                        ]
                    , tGroup
                        ":="
                        [ testExpectSuccess "eta !$ ap{e1} := \"hello\""
                        , testExpectSuccess "eea !$ ap{e1} := e2"
                        , testExpectSuccess "eea !$ ap{e1} := e2 >> testrefeq ap{e2} (eea !$ ap{e1})"
                        , testExpectSuccess "eta !$ ap{e1} := \"hello\" >> testrefeq ap{\"hello\"} (eta !$ ap{e1})"
                        , testExpectSuccess "tea !$ ap{\"hello\"} := e1 >> testrefeq ap{e1} (tea !$ ap{\"hello\"})"
                        , testExpectSuccess
                            "tea !$ ap{\"hello\"} := e1 >> testrefeq ap{1} (count.FiniteSetModel (tea !@ ap{e1}))"
                        , testExpectSuccess "(eea ..Property eea) !$ ap{e1} := e2"
                        , testExpectSuccess
                            "do {(eea ..Property eea) !$ ap{e1} := e2; testrefeq ap{e2} ((eea ..Property eea) !$ ap{e1}); testrefeq ap{e2} (eea !$ (eea !$ ap{e1}));}"
                        , testExpectSuccess
                            "do {eea !$ (eea !$ ap{e1}) := e2; testrefeq ap{e2} ((eea ..Property eea) !$ ap{e1}); testrefeq ap{e2} (eea !$ (eea !$ ap{e1}));}"
                        , testExpectSuccess "expectStop $ do {r <- newMem.WholeModel; eia !$ r := 4;}"
                        ]
                    , tGroup
                        "+="
                        [ testExpectSuccess "eta !@ ap{\"hello\"} += e1"
                        , testExpectSuccess "eta !@ ap{\"hello\"} += e1 >> pass"
                        , testExpectSuccess "eta !@ ap{\"hello\"} += e1 >> testrefeq ap{\"hello\"} (eta !$ ap{e1})"
                        ]
                    , tGroup
                        "-="
                        [ testExpectSuccess
                            "eta !@ ap{\"hello\"} += e1 >> eta !@ ap{\"hello\"} -= e1 >> testrefisunknown (eta !$ ap{e1})"
                        ]
                    , tGroup
                        "clear.FiniteSetModel"
                        [ testExpectSuccess
                            "eta !@ ap{\"hello\"} += e1 >> clear.FiniteSetModel (eta !@ ap{\"hello\"}) >> testrefisunknown (eta !$ ap{e1})"
                        ]
                    , tDecls ["c1 = !{cell @Boolean !\"c1\"} store"]
                        $ tGroup
                            "cell"
                            [ testExpectSuccess "c1 := True >> testrefeq ap{True} c1"
                            , testExpectSuccess "c1 := False >> testrefeq ap{False} c1"
                            ]
                    , tDecls ["s1 = !{set @Integer !\"s1\"} store"]
                        $ tGroup
                            "set"
                            [ testExpectSuccess "testrefeq ap{[]} (toList.FiniteSetModel order.Integer s1)"
                            , testExpectSuccess "s1 += 2 >> testrefeq ap{[2]} (toList.FiniteSetModel order.Integer s1)"
                            , testExpectSuccess
                                "s1 += 5 >> s1 += 3 >> testrefeq ap{[3,5]} (toList.FiniteSetModel order.Integer s1)"
                            ]
                    , tGroup
                        "type"
                        [ testExpectSuccess "let {c = !{cell @Boolean !\"c\"} store} pass"
                        , testExpectSuccess "let {c = !{cell @Unit !\"c\"} store} pass"
                        , testExpectSuccess "let {c = !{cell @(Maybe Integer) !\"c\"} store} pass"
                        , testExpectSuccess "let {c = !{cell @(List Unit) !\"c\"} store} pass"
                        , testExpectSuccess "let {c = !{cell @(List (List (List Integer))) !\"c\"} store} pass"
                        ]
                    , tDecls
                        [ "longtext = \"jfkljgkljrklgjkvbnvleriirejgioerjhgitrklnmbdfmkl;dmnverireigjerkgjrevjkrljvkljvklsjvroejrgiojgireojg\""
                        , "ela = !{property @E @(List Integer) !\"ela\"} store"
                        ]
                        $ tGroup
                            "fetch"
                            [ testExpectSuccess "pass"
                            , testExpectSuccess
                                "do {v <- !{fetch @Text} store \"hvfjkhvjrkes\"; testrefeq ap{\"hvfjkhvjrkes\"} ap{v}}"
                            , testExpectSuccess
                                "expectStop $ do {v <- !{fetch @Text} store longtext; testrefeq ap{longtext} ap{v}}"
                            , testExpectSuccess
                                "do {eta !$ ap{e1} := longtext; v <- !{fetch @Text} store longtext; testrefeq ap{longtext} ap{v}}"
                            , testExpectSuccess
                                "expectStop $ do {v <- !{fetch @Text} store [3,4,5]; testrefeq ap{[3,4,5]} ap{v}}"
                            , testExpectSuccess
                                "do {ela !$ ap{e1} := [3,4,5]; v <- get $ ela !$ ap{e1}; testrefeq ap{[3,4,5]} ap{v}}"
                            , testExpectSuccess
                                "do {ela !$ ap{e1} := [3,4,5]; v <- !{fetch @(List Integer)} store [3,4,5]; testrefeq ap{[3,4,5]} ap{v}}"
                            ]
                    , tGroup
                        "literal storage"
                        [ tGroup
                            "Boolean"
                            [ testExpectSuccess "eba !$ ap{e1} := True >> testrefeq ap{True} (eba !$ ap{e1})"
                            , testExpectSuccess "eba !$ ap{e1} := False >> testrefeq ap{False} (eba !$ ap{e1})"
                            ]
                        , tGroup
                            "Text"
                            [ testExpectSuccess "eta !$ ap{e1} := \"\" >> testrefeq ap{\"\"} (eta !$ ap{e1})"
                            , testExpectSuccess "eta !$ ap{e1} := \"hello\" >> testrefeq ap{\"hello\"} (eta !$ ap{e1})"
                            ]
                        , tGroup
                            "Integer"
                            [ testExpectSuccess "eia !$ ap{e1} := 0 >> testrefeq ap{0} (eia !$ ap{e1})"
                            , testExpectSuccess "eia !$ ap{e1} := 47 >> testrefeq ap{47} (eia !$ ap{e1})"
                            , testExpectSuccess "eia !$ ap{e1} := -12 >> testrefeq ap{-12} (eia !$ ap{e1})"
                            ]
                        , tGroup
                            "Rational"
                            [ testExpectSuccess "era !$ ap{e1} := 0 >> testrefeq ap{0} (era !$ ap{e1})"
                            , testExpectSuccess "era !$ ap{e1} := 47 >> testrefeq ap{47} (era !$ ap{e1})"
                            , testExpectSuccess "era !$ ap{e1} := -12 >> testrefeq ap{-12} (era !$ ap{e1})"
                            , testExpectSuccess "era !$ ap{e1} := 31.5 >> testrefeq ap{31.5} (era !$ ap{e1})"
                            , testExpectSuccess "era !$ ap{e1} := -22.8_70 >> testrefeq ap{-22.8_70} (era !$ ap{e1})"
                            ]
                        , tGroup
                            "Number"
                            [ testExpectSuccess "ena !$ ap{e1} := 0 >> testrefeq ap{0} (ena !$ ap{e1})"
                            , testExpectSuccess "ena !$ ap{e1} := 47 >> testrefeq ap{47} (ena !$ ap{e1})"
                            , testExpectSuccess "ena !$ ap{e1} := -12 >> testrefeq ap{-12} (ena !$ ap{e1})"
                            , testExpectSuccess "ena !$ ap{e1} := 31.5 >> testrefeq ap{31.5} (ena !$ ap{e1})"
                            , testExpectSuccess "ena !$ ap{e1} := -22.8_70 >> testrefeq ap{-22.8_70} (ena !$ ap{e1})"
                            , testExpectSuccess "ena !$ ap{e1} := ~36.4 >> testrefeq ap{~36.4} (ena !$ ap{e1})"
                            , testExpectSuccess "ena !$ ap{e1} := ~-22.1 >> testrefeq ap{~-22.1} (ena !$ ap{e1})"
                            , testExpectSuccess "ena !$ ap{e1} := ~-0 >> testrefeq ap{~-0} (ena !$ ap{e1})"
                            , testExpectSuccess
                                "ena !$ ap{e1} := (0 /.Number 0) >> testrefeq ap{0 /.Number 0} (ena !$ ap{e1})"
                            , testExpectSuccess
                                "ena !$ ap{e1} := (1 /.Number 0) >> testrefeq ap{1 /.Number 0} (ena !$ ap{e1})"
                            , testExpectSuccess
                                "ena !$ ap{e1} := (-1 /.Number 0) >> testrefeq ap{-1 /.Number 0} (ena !$ ap{e1})"
                            ]
                        ]
                    , tGroup
                        "matching literals"
                        [ testExpectSuccess
                            "eta !$ ap{e1} := \"hello\" >> eta !$ ap{e2} := \"hello\" >> testrefeq (eta !$ ap{e1}) (eta !$ ap{e2})"
                        ]
                    , tGroup
                        "identity property"
                        [ testExpectSuccess "(id.Property !$ eea !$ ap{e1}) := e2 >> testrefeq ap{e2} (eea !$ ap{e1})"
                        , testExpectSuccess "(eea !$ id.Property !$ ap{e1}) := e2 >> testrefeq ap{e2} (eea !$ ap{e1})"
                        , testExpectSuccess
                            "((id.Property ..Property eea) !$ ap{e1}) := e2 >> testrefeq ap{e2} (eea !$ ap{e1})"
                        , testExpectSuccess
                            "((eea ..Property id.Property) !$ ap{e1}) := e2 >> testrefeq ap{e2} (eea !$ ap{e1})"
                        , testExpectSuccess "eea !$ ap{e1} := e2 >> testrefeq ap{e2} (id.Property !$ eea !$ ap{e1})"
                        , testExpectSuccess "eea !$ ap{e1} := e2 >> testrefeq ap{e2} (eea !$ id.Property !$ ap{e1})"
                        , testExpectSuccess
                            "eea !$ ap{e1} := e2 >> testrefeq ap{e2} ((id.Property ..Property eea) !$ ap{e1})"
                        , testExpectSuccess
                            "eea !$ ap{e1} := e2 >> testrefeq ap{e2} ((eea ..Property id.Property) !$ ap{e1})"
                        , testExpectSuccess
                            "(id.Property !$ eea !$ ap{e1}) := e2 >> testrefeq ap{e2} (id.Property !$ eea !$ ap{e1})"
                        ]
                    , tGroup
                        "identity inverse property"
                        [ testExpectSuccess
                            "(id.Property !@@ eta !@ ap{\"hello\"}) += e1 >> testrefeq ap{\"hello\"} (eta !$ ap{e1})"
                        , testExpectSuccess "(eea !@@ id.Property !@ ap{e2}) += e1 >> testrefneq ap{e2} (eea !$ ap{e1})"
                        , testExpectSuccess "(eta !@ ap{\"hello\"}) += e1 >> testrefeq ap{\"hello\"} (eta !$ ap{e1})"
                        , testExpectSuccess
                            "((id.Property ..Property eta) !@ ap{\"hello\"}) += e1 >> testrefeq ap{\"hello\"} (eta !$ ap{e1})"
                        , testExpectSuccess
                            "((eta ..Property id.Property) !@ ap{\"hello\"}) += e1 >> testrefeq ap{\"hello\"} (eta !$ ap{e1})"
                        , testExpectSuccess
                            "eta !@ ap{\"hello\"} += e1 >> eta !@ ap{\"hello\"} -= e1 >> testrefisunknown (eta !$ ap{e1})"
                        , testExpectSuccess
                            "eta !@ ap{\"hello\"} += e1 >> (id.Property !@@ eta !@ ap{\"hello\"}) -= e1 >> testrefeq ap{\"hello\"} (eta !$ ap{e1})"
                        , testExpectSuccess "eea !@ ap{e2} += e1 >> testrefeq ap{e2} (eea !$ ap{e1})"
                        , testExpectSuccess
                            "eea !@ ap{e2} += e1 >> (eea !@@ id.Property !@ ap{e2}) -= e1 >> testrefneq ap{e2} (eea !$ ap{e1})"
                        , testExpectSuccess
                            "eta !@ ap{\"hello\"} += e1 >> ((id.Property ..Property eta) !@ ap{\"hello\"}) -= e1 >> testrefisunknown (eta !$ ap{e1})"
                        , testExpectSuccess
                            "eta !@ ap{\"hello\"} += e1 >> ((eta ..Property id.Property) !@ ap{\"hello\"}) -= e1 >> testrefisunknown (eta !$ ap{e1})"
                        ]
                    , tGroup
                        "composed properties"
                        [ testExpectSuccess "(eea !$ eeb !$ ap{e1}) := e2 >> testrefeq ap{e2} (eea !$ eeb !$ ap{e1})"
                        , testExpectSuccess
                            "(eta !$ eeb !$ ap{e1}) := \"hello\" >> testrefeq ap{\"hello\"} (eta !$ eeb !$ ap{e1})"
                        , testExpectSuccess
                            "(eea ..Property eeb !$ ap{e1}) := e2 >> testrefeq ap{e2} (eea !$ eeb !$ ap{e1})"
                        , testExpectSuccess
                            "(eta ..Property eeb !$ ap{e1}) := \"hello\" >> testrefeq ap{\"hello\"} (eta !$ eeb !$ ap{e1})"
                        , testExpectSuccess
                            "(eea !$ eeb !$ ap{e1}) := e2 >> testrefeq ap{e2} (eea ..Property eeb !$ ap{e1})"
                        , testExpectSuccess
                            "(eta !$ eeb !$ ap{e1}) := \"hello\" >> testrefeq ap{\"hello\"} (eta ..Property eeb !$ ap{e1})"
                        , testExpectSuccess
                            "(eeb ..Property eea) !$ ap{e2} := e1 >> testrefeq ap{e1} (eeb !$ eea !$ ap{e2})"
                        ]
                    , tGroup
                        "composed inverse properties"
                        [ testExpectSuccess
                            "(eeb !@@ eta !@ ap{\"hello\"}) += e1 >> testrefeq ap{\"hello\"} (eta !$ eeb !$ ap{e1})"
                        , testExpectSuccess
                            "((eta ..Property eeb) !@ ap{\"hello\"}) += e1 >> testrefeq ap{\"hello\"} (eta !$ eeb !$ ap{e1})"
                        , testExpectSuccess
                            "((eta ..Property eeb) !@ ap{\"hello\"}) += e1 >> testrefisunknown (eta !$ ap{e1})"
                        , testExpectSuccess
                            "eeb !$ ap{e1} := e2 >> ((eta ..Property eeb) !@ ap{\"hello\"}) += e1 >> testrefeq ap{e2} (eeb !$ ap{e1})"
                        , testExpectSuccess
                            "eeb !$ ap{e1} := e2 >> ((eta ..Property eeb) !@ ap{\"hello\"}) += e1 >> testrefeq ap{\"hello\"} (eta !$ ap{e2})"
                        , testExpectSuccess
                            "eeb !$ ap{e1} := e2 >> (eeb !@@ eta !@  ap{\"hello\"}) += e1 >> testrefeq ap{e2} (eeb !$ ap{e1})"
                        , testExpectSuccess
                            "eeb !$ ap{e1} := e2 >> (eeb !@@ eta !@  ap{\"hello\"}) += e1 >> testrefeq ap{\"hello\"} (eta !$ ap{e2})"
                        , testExpectSuccess
                            "eeb !$ ap{e1} := e2 >> eta !$ ap{e2} := \"hello\" >> ((eta ..Property eeb) !@ ap{\"hello\"}) -= e1 >> testrefeq ap{e2} (eeb !$ ap{e1})"
                        , testExpectSuccess
                            "eeb !$ ap{e1} := e2 >> eta !$ ap{e2} := \"hello\" >> ((eta ..Property eeb) !@ ap{\"hello\"}) -= e1 >> testrefisunknown (eta !$ ap{e2})"
                        , testExpectSuccess
                            "eeb !$ ap{e1} := e2 >> eta !$ ap{e2} := \"hello\" >> (eeb !@@ eta !@ ap{\"hello\"}) -= e1 >> testrefneq ap{e2} (eeb !$ ap{e1})"
                        , testExpectSuccess
                            "eeb !$ ap{e1} := e2 >> eta !$ ap{e2} := \"hello\" >> (eeb !@@ eta !@ ap{\"hello\"}) -= e1 >> testrefeq ap{\"hello\"} (eta !$ ap{e2})"
                        , testExpectSuccess
                            "eeb !$ ap{e1} := e2 >> eta !$ ap{e2} := \"hello\" >> clear.FiniteSetModel ((eta ..Property eeb) !@ ap{\"hello\"}) >> testrefeq ap{e2} (eeb !$ ap{e1})"
                        , testExpectSuccess
                            "eeb !$ ap{e1} := e2 >> eta !$ ap{e2} := \"hello\" >> clear.FiniteSetModel ((eta ..Property eeb) !@ ap{\"hello\"}) >> testrefisunknown (eta !$ ap{e2})"
                        , testExpectSuccess
                            "eeb !$ ap{e1} := e2 >> eta !$ ap{e2} := \"hello\" >> clear.FiniteSetModel (eeb !@@ eta !@ ap{\"hello\"}) >> testrefneq ap{e2} (eeb !$ ap{e1})"
                        , testExpectSuccess
                            "eeb !$ ap{e1} := e2 >> eta !$ ap{e2} := \"hello\" >> clear.FiniteSetModel (eeb !@@ eta !@ ap{\"hello\"}) >> testrefeq ap{\"hello\"} (eta !$ ap{e2})"
                        ]
                    , tGroup
                        "single.FiniteSetModel"
                        [ testExpectSuccess "testrefisunknown (single.FiniteSetModel $ eib !$$% eia !@ ap{0})"
                        , testExpectSuccess
                            "eib !$ ap{e1} := 1 >> eia !$ ap{e1} := 0 >> testrefeq ap{1} (single.FiniteSetModel $ eib !$$% eia !@ ap{0})"
                        , testExpectSuccess
                            "eib !$ ap{e1} := 1 >> eia !$ ap{e1} := 0 >> eic !$ ap{e1} := 0 >> testrefeq ap{1} (single.FiniteSetModel $ eib !$$% eia !@ ap{0})"
                        , testExpectSuccess
                            "eib !$ ap{e1} := 1 >> eia !$ ap{e1} := 0 >> eia !$ ap{e1} := 0 >> testrefeq ap{1} (single.FiniteSetModel $ eib !$$% eia !@ ap{0})"
                        , testExpectSuccess
                            "eib !$ ap{e1} := 1 >> eib !$ ap{e2} := 2 >> eia !$ ap{e1} := 0 >> eia !$ ap{e2} := 0 >> testrefisunknown (single.FiniteSetModel $ eib !$$% eia !@ ap{0})"
                        , testExpectSuccess
                            "eib !$ ap{e1} := 1 >> eib !$ ap{e2} := 1 >> eia !$ ap{e1} := 0 >> eia !$ ap{e2} := 0 >> testrefeq ap{1} (single.FiniteSetModel $ eib !$$% eia !@ ap{0})"
                        ]
                    , tGroup
                        "multiple set member"
                        [ testExpectSuccess "testrefeq ap{0} (count.FiniteSetModel (tea !@ ap{e1}))"
                        , testExpectSuccess "eea !$ ap{e2} := e1 >> testrefeq ap{1} (count.FiniteSetModel (eea !@ ap{e1}))"
                        , testExpectSuccess "eea !@ ap{e1} += e2 >> testrefeq ap{1} (count.FiniteSetModel (eea !@ ap{e1}))"
                        , testExpectSuccess "tea !$ ap{\"hello\"} := e1 >> testrefeq ap{e1} (tea !$ ap{\"hello\"})"
                        , testExpectSuccess "tea !@ ap{e1} += \"hello\" >> testrefeq ap{e1} (tea !$ ap{\"hello\"})"
                        , testExpectSuccess
                            "tea !$ ap{\"hello\"} := e1 >> testrefeq ap{1} (count.FiniteSetModel (tea !@ ap{e1}))"
                        , testExpectSuccess
                            "tea !@ ap{e1} += \"hello\" >> testrefeq ap{1} (count.FiniteSetModel (tea !@ ap{e1}))"
                        , testExpectSuccess
                            "tea !@ ap{e1} += \"hello\" >> tea !@ ap{e1} += \"hello\" >> testrefeq ap{1} (count.FiniteSetModel (tea !@ ap{e1}))"
                        , testExpectSuccess
                            "tea !@ ap{e1} += \"h\" >> tea !@ ap{e1} += \"hello\" >> testrefeq ap{2} (count.FiniteSetModel (tea !@ ap{e1}))"
                        , testExpectSuccess
                            $ "let {counter = eia !$ ap{e1};someset = nea !@ ap{e1}} "
                            <> "counter := 0 >> someset += 1 >> someset += 1 >> (get (toList.FiniteSetModel empty.Order someset) >>= fn pp => for pp $ fn p => runWholeModel ap{counter := succ.Integer %counter}) >> testrefeq ap{1} counter"
                        ]
                    , tGroup
                        "types"
                        [ testExpectSuccess "let {entitytype T1; p = property @T1 @T1 !\"p\"} pass"
                        , testExpectSuccess "let {entitytype T1} let {p = property @T1 @T1 !\"p\"} pass"
                        , testExpectSuccess "let {entitytype T1; entitytype T2; p = property @T1 @T2 !\"p\"} pass"
                        , testExpectSuccess "let {entitytype T1; entitytype T2} let {p = property @T1 @T2 !\"p\"} pass"
                        , testExpectSuccess "let {entitytype T1} let {entitytype T2; p = property @T1 @T2 !\"p\"} pass"
                        , testExpectSuccess "let {entitytype T1} let {entitytype T2} let {p = property @T1 @T2 !\"p\"} pass"
                        , testExpectReject "let {entitytype T1} let {entitytype T1} pass"
                        , testExpectReject "let {entitytype T1; entitytype T1} pass"
                        ]
                    , tGroup
                        "Maybe"
                        [ testExpectSuccess
                            "let {enta = !{property @E @(Maybe Text) !\"enta\"} store} enta !$ ap{e1} := Just \"abc\" >> (testrefeq ap{Just \"abc\"} $ enta !$ ap{e1})"
                        , testExpectSuccess
                            "let {enta = !{property @E @(Maybe Text) !\"enta\"} store} enta !$ ap{e1} := Nothing >> (testrefeq ap{Nothing} $ enta !$ ap{e1})"
                        ]
                    , tGroup
                        "List"
                        [ testExpectSuccess
                            "let {enta = !{property @E @(List Text) !\"enta\"} store} enta !$ ap{e1} := [\"abc\", \"def\"] >> (testrefeq ap{[\"abc\", \"def\"]} $ enta !$ ap{e1})"
                        , testExpectSuccess
                            "let {enta = !{property @E @(List Text) !\"enta\"} store} enta !$ ap{e1} := [] >> (testrefeq ap{[]} $ enta !$ ap{e1})"
                        ]
                    , tGroup
                        "Pair/Either"
                        [ testExpectSuccess
                            "let {enta = !{property @E @(Number *: Text) !\"enta\"} store} enta !$ ap{e1} := (74,\"hmm\") >> (testrefneq ap{(71,\"hmm\")} $ enta !$ ap{e1})"
                        , testExpectSuccess
                            "let {enta = !{property @E @(Number *: Text) !\"enta\"} store} enta !$ ap{e1} := (74,\"hmm\") >> (testrefeq ap{(74,\"hmm\")} $ enta !$ ap{e1})"
                        , testExpectSuccess
                            "let {enta = !{property @E @(Number +: Text) !\"enta\"} store} enta !$ ap{e1} := Left 74 >> (testrefneq ap{Left 73} $ enta !$ ap{e1})"
                        , testExpectSuccess
                            "let {enta = !{property @E @(Number +: Text) !\"enta\"} store} enta !$ ap{e1} := Left 74 >> (testrefeq ap{Left 74} $ enta !$ ap{e1})"
                        , testExpectSuccess
                            "let {enta = !{property @E @(Number +: Text) !\"enta\"} store} enta !$ ap{e1} := Right \"abc\" >> (testrefneq ap{Right \"adbc\"} $ enta !$ ap{e1})"
                        , testExpectSuccess
                            "let {enta = !{property @E @(Number +: Text) !\"enta\"} store} enta !$ ap{e1} := Right \"abc\" >> (testrefeq ap{Right \"abc\"} $ enta !$ ap{e1})"
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
                        [ subtypeTest False SRSingle "Order a" "ModelOrder a"
                        , subtypeTest False SRSingle "Order Integer" "ModelOrder Integer"
                        ]
                    , tGroup
                        "models"
                        [ subtypeTest False SRSingle "ListModel a" "WholeModel (List a)"
                        , subtypeTest False SRSingle "ListModel Integer" "WholeModel (List Integer)"
                        , subtypeTest False SRSingle "ListModel Integer" "WholeModel (-List Integer,+List Integer)"
                        , subtypeTest
                            True
                            SRSingle
                            "ListModel (-(a & Integer),+(a | Integer))"
                            "WholeModel (List Integer)"
                        , subtypeTest
                            True
                            SRSingle
                            "ListModel (-(a & Entity),+(a | Integer))"
                            "WholeModel (List Integer)"
                        , tGroup
                            "Model"
                            [ subtypeTest False SRSingle "WholeModel (-Integer,+Text)" "Model"
                            , subtypeTest False SRSingle "ListModel (-Text,+Text)" "Model"
                            , subtypeTest False SRSingle "ListModel (-Integer,+Text)" "Model"
                            , subtypeTest False SRSingle "TextModel " "Model"
                            , subtypeTest False SRSingle "SetModel Text" "Model"
                            , subtypeTest False SRSingle "FiniteSetModel (-Integer,+Text)" "Model"
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
                    [ tDecls ["entitytype P", "entitytype Q", "subtype P <: Q"]
                        $ tGroup "seq"
                        $ strictSubtypeTests "P" "Q"
                    , tDeclsRec ["entitytype P", "entitytype Q", "subtype P <: Q"]
                        $ tGroup "rec 1"
                        $ strictSubtypeTests "P" "Q"
                    , tDeclsRec ["entitytype P", "subtype P <: Q", "entitytype Q"]
                        $ tGroup "rec 2"
                        $ strictSubtypeTests "P" "Q"
                    , tDeclsRec ["subtype P <: Q", "entitytype P", "entitytype Q"]
                        $ tGroup "rec 3"
                        $ strictSubtypeTests "P" "Q"
                    ]
                , tGroup
                    "local"
                    [ tDecls ["entitytype P"]
                        $ tGroup
                            "1"
                            [ testExpectSuccess "pass"
                            , testExpectSuccess "let {entitytype Q; subtype P <: Q} pass"
                            , testExpectSuccess "let {entitytype Q; subtype P <: Q; f : P -> Q = fn x => x} pass"
                            , testExpectReject "let {entitytype Q; subtype P <: Q; f : Q -> P = fn x => x} pass"
                            ]
                    , tDecls ["entitytype Q"]
                        $ tGroup
                            "2"
                            [ testExpectSuccess "pass"
                            , testExpectSuccess "let {entitytype P; subtype P <: Q} pass"
                            , testExpectSuccess "let {entitytype P; subtype P <: Q; f : P -> Q = fn x => x} pass"
                            , testExpectReject "let {entitytype P; subtype P <: Q; f : Q -> P = fn x => x} pass"
                            ]
                    , tDecls ["entitytype P", "entitytype Q"]
                        $ tGroup
                            "3"
                            [ testExpectSuccess "pass"
                            , testExpectSuccess "let {subtype P <: Q} pass"
                            , testExpectSuccess "let {subtype P <: Q; f : P -> Q = fn x => x} pass"
                            , testExpectReject "let {subtype P <: Q; f : Q -> P = fn x => x} pass"
                            ]
                    ]
                , tGroup
                    "circular"
                    [ tDecls ["entitytype P", "subtype P <: P"]
                        $ tGroup
                            "singular"
                            [ testExpectSuccess "pass"
                            , testExpectSuccess "let {f : P -> P = fn x => x} pass"
                            , testExpectSuccess "let {f : List P -> List P = fn x => x} pass"
                            ]
                    , tDecls ["entitytype P", "entitytype Q", "subtype P <: Q", "subtype Q <: P"]
                        $ tGroup
                            "pair"
                            [ testExpectSuccess "pass"
                            , testExpectSuccess "let {f : P -> P = fn x => x} pass"
                            , testExpectSuccess "let {f : Q -> Q = fn x => x} pass"
                            , testExpectSuccess "let {f : P -> Q = fn x => x} pass"
                            , testExpectSuccess "let {f : List P -> List Q = fn x => x} pass"
                            , testExpectSuccess "let {f : Q -> P = fn x => x} pass"
                            ]
                    ]
                , tDecls
                    [ "subtype EntityMap a <: Entity -> Maybe a = fn m, e => lookup.EntityMap e m"
                    , "x = single.EntityMap 34 \"sometext\""
                    ]
                    $ tGroup "EntityMap" [testExpectSuccess "pass", testExpectSuccess "testeq (Just \"sometext\") $ x 34"]
                , tDecls ["entitytype Q", "subtype Maybe Number <: Q"]
                    $ tGroup
                        "non-simple" -- not allowed, per issue #28
                        [testExpectReject "pass"]
                , tDecls ["entitytype Q", "subtype Integer <: Q"] $ tGroup "literal" $ strictSubtypeTests "Integer" "Q"
                , tDecls ["entitytype Q", "datatype storable P {P1 Text Number !\"P.P1\"}", "subtype P <: Q"]
                    $ tGroup "closed"
                    $ strictSubtypeTests "P" "Q"
                , tDecls
                    [ "entitytype Q"
                    , "entitytype R"
                    , "datatype storable P {P1 Text Number !\"P.P1\"}"
                    , "subtype P <: Q"
                    , "subtype P <: R"
                    ]
                    $ tGroup "closed"
                    $ strictSubtypeTests "P" "R"
                , tGroup
                    "Entity"
                    [ testExpectSuccess "let {f : Number -> Entity = fn x => x} pass"
                    , testExpectSuccess "let {f : (a & Number) -> Entity *: a = fn x => (x,x)} pass"
                    , testExpectSuccess "let {f : Maybe Number -> Entity = fn x => x} pass"
                    , testExpectSuccess "let {f : Maybe (a & Number) -> Entity *: Maybe a = fn x => (x,x)} pass"
                    ]
                , tGroup
                    "equivalent"
                    [ tDecls ["type I = Integer"]
                        $ tGroup
                            "simple"
                            [ testExpectSuccess "pass"
                            , testExpectSuccess "testeq (3: Integer) (3: I)"
                            , testExpectSuccess "testeq 7 (7: I : Integer : I : Integer)"
                            ]
                    , tOpenDefaultStore
                        $ tWith ["Store"]
                        $ tDecls
                            [ "type storable Point = Number *: Number"
                            , "type storable PM +a = a *: Maybe a"
                            , "cellPoint = !{cell @Point !\"cPoint\"} store"
                            , "cellT = !{cell @(PM Text) !\"cT\"} store"
                            ]
                        $ tGroup
                            "storable"
                            [ testExpectSuccess "pass"
                            , testExpectSuccess "do {cellPoint := (3.5,4); testrefeq ap{(3.5,4)} cellPoint}"
                            , testExpectSuccess "do {cellT := (\"P\",Just \"Q\"); testrefeq ap{(\"P\",Just \"Q\")} cellT}"
                            ]
                    , tGroup
                        "param"
                        [ tDecls
                            [ "type M +a = Maybe a"
                            ]
                            $ tGroup
                                "covariant"
                                [ testExpectSuccess "pass"
                                , testExpectSuccess "testeq (Just 51: M Integer) (Just 51: Maybe Integer)"
                                , testExpectSuccess "testeq ((Just 17: M Integer): M Rational) (Just 17: Maybe Integer)"
                                , testExpectSuccess "testeq ((Just 84: M Integer): M Rational) (Just 84: Maybe Rational)"
                                ]
                        , tDecls
                            [ "type Endo (-p,+q) = p -> q"
                            , "double : Endo Integer = fn x => x +.Integer x"
                            ]
                            $ tGroup
                                "pair-separate"
                                [ testExpectSuccess "pass"
                                , testExpectSuccess "testeq (double $ double $ double 7) 56"
                                ]
                        , tDecls
                            [ "type Endo (-p,+q) = q -> p"
                            ]
                            $ tGroup
                                "inverted"
                                [ testExpectReject "pass"
                                ]
                        , tDecls
                            [ "datatype Endo a {Mk (a -> a)}"
                            , "unEndo = fn Mk.Endo x => x"
                            , "double : Endo Integer = Mk.Endo $ fn x => x +.Integer x"
                            ]
                            $ tGroup
                                "datatype-pair-double"
                                [ testExpectSuccess "pass"
                                , testExpectSuccess "testeq (unEndo double $ unEndo double $ unEndo double 7) 56"
                                ]
                        , tDecls
                            [ "type Endo a = a -> a"
                            , "double : Endo Integer = fn x => x +.Integer x"
                            ]
                            $ tGroup
                                "pair-double"
                                [ testExpectSuccess "pass"
                                , testExpectSuccess "testeq (double $ double $ double 7) 56"
                                ]
                        ]
                    , tDecls
                        [ "type ML +a = Maybe (List a)"
                        , "fa: Maybe (List a) -> ML a = fn x => x"
                        , "ga: ML a -> Maybe (List a) = fn x => x"
                        , "fga = fn x => fa (ga x)"
                        , "gfa = fn x => ga (fa x)"
                        , "fi: Maybe (List Integer) -> ML Integer = fn x => x"
                        , "gi: ML Integer -> Maybe (List Integer) = fn x => x"
                        , "fgi = fn x => fi (gi x)"
                        , "gfi = fn x => gi (fi x)"
                        ]
                        $ tModify (failTestBecause "??")
                        $ tGroup
                            "nested"
                            [ testExpectSuccess "pass"
                            , testExpectSuccess "testeq (Just [12]: Maybe (List Integer)) (Just [12]: Maybe (List Integer))"
                            , testExpectSuccess "testeq (Just [12]: ML Integer) (Just [12]: ML Integer)"
                            , testExpectSuccess "testeq (Just [55]) (gfa (Just [55]))"
                            , testExpectSuccess "testeq (Just [57]) (gfi (Just [57]))"
                            , testExpectSuccess "(Just [74]: ML Integer) >- fn {Nothing => fail \"Nothing\"; Just a => testeq [74] a;}"
                            , testExpectSuccess "testeq (Just [12]: ML Integer) (Just [12]: Maybe (List Integer))"
                            , testExpectSuccess "testeq ((Just [12]: ML Integer): Maybe (List Integer)) (Just [12]: Maybe (List Integer))"
                            , testExpectSuccess "testeq (((Just [12]: Maybe (List Integer)): ML Integer): Maybe (List Integer)) (Just [12]: Maybe (List Integer))"
                            , testExpectSuccess "testeq ((Just [92]: ML Integer): ML Rational) (Just [92]: Maybe (List Integer))"
                            , testExpectSuccess "testeq ((Just [61]: ML Integer): ML Rational) (Just [61]: Maybe (List Rational))"
                            ]
                    ]
                , tDecls
                    [ "testIsJust = fn {Just _ => pass; Nothing => fail \"Nothing\"}"
                    , "testIsNothing = fn {Nothing => pass; Just _ => fail \"Just\"}"
                    ]
                    $ tGroup
                        "predicate"
                        [ tDecls ["predicatetype Even <: Integer = fn i => mod i 2 == 0"]
                            $ tGroup
                                "Even"
                                [ testExpectSuccess "pass"
                                , tGroup "Even <: Integer" $ strictSubtypeTests "Even" "Integer"
                                , tGroup "Even <: Literal" $ strictSubtypeTests "Even" "Literal"
                                , testExpectSuccess "testrefeq ap{Just 4} ap{!{check @Even} 4}"
                                , testExpectSuccess "testrefeq ap{Nothing} ap{!{check @Even} 5}"
                                ]
                        , tDecls ["predicatetype F <: Integer -> Integer = fn f => f 3 == 4"]
                            $ tGroup
                                "Integer -> Integer"
                                [ testExpectSuccess "pass"
                                , testExpectSuccess "testIsJust $ !{check @F} $ fn x => x + 1"
                                , testExpectSuccess "testIsNothing $ !{check @F} $ fn x => x"
                                , testExpectSuccess
                                    "testrefeq ap{Just 6} ap{map.Maybe (fn f => f 5) $ !{check @F} $ fn x => x + 1}"
                                ]
                        , testExpectReject "let {predicatetype F <: a -> a = fn f => True} pass"
                        , tGroup
                            "unroll"
                            [ testExpectSuccess
                                "let {predicatetype AtLeastThree <: Maybe (rec a, Maybe a) = fn {Just (Just (Just _)) => True; _ => False;}} pass"
                            , testExpectSuccess
                                "let {predicatetype AtLeastThree <: rec a, Maybe a = fn {Just (Just (Just _)) => True; _ => False;}} pass"
                            , testExpectSuccess
                                "let {predicatetype AtLeastThree <: rec a, a +: a = fn {Left (Left (Left _)) => True; _ => False;}} pass"
                            , testExpectSuccess
                                "let {predicatetype AtLeastThree <: rec a, rec b, a +: b = fn {Left (Left (Left _)) => True; _ => False;}} pass"
                            , testExpectReject "let {predicatetype Degenerate <: rec a, a = fn _ => True} pass"
                            ]
                        , tGroup
                            "storable"
                            [ testExpectSuccess "let {predicatetype F <: Integer -> Integer = fn _ => True} pass"
                            , testExpectReject "let {predicatetype storable F <: Integer -> Integer = fn _ => True} pass"
                            , tOpenDefaultStore
                                $ tWith ["Store"]
                                $ tDecls
                                    [ "predicatetype storable Even <: Integer = fn i => mod i 2 == 0"
                                    , "cellInteger = !{cell @Integer !\"c\"} store"
                                    , "cellEven = !{cell @Even !\"c\"} store"
                                    ]
                                $ tGroup
                                    "store"
                                    [ testExpectSuccess "pass"
                                    , testExpectSuccess "do {cellInteger := 18; testrefeq ap{18} cellInteger}"
                                    , testExpectSuccess "do {cellInteger := 18; testrefeq ap{18} cellEven}"
                                    , testExpectSuccess "do {cellInteger := 17; testrefeq ap{17} cellInteger}"
                                    , testExpectSuccess "do {cellInteger := 17; testrefisunknown cellEven}"
                                    ]
                            ]
                        ]
                ]
            , tGroup
                "greatest-dynamic-supertype"
                [ tGroup
                    "Literal"
                    [ testExpectSuccess "testeq 1 $ 34.0 >- fn {34 => 1; True => 2; \"hello\" => 3; _ => 4}"
                    , testExpectSuccess "testeq 2 $ True >- fn {34 => 1; True => 2; \"hello\" => 3; _ => 4}"
                    , testExpectSuccess "testeq 3 $ \"hello\" >- fn {34 => 1; True => 2; \"hello\" => 3; _ => 4}"
                    , testExpectSuccess "testeq 1 $ 34.0 >- fn {_:?Integer => 1; _:?Boolean => 2; _:?Text => 3; _ => 4}"
                    , testExpectSuccess "testeq 2 $ True >- fn {_:?Integer => 1; _:?Boolean => 2; _:?Text => 3; _ => 4}"
                    , testExpectSuccess
                        "testeq 3 $ \"hello\" >- fn {_:?Integer => 1; _:?Boolean => 2; _:?Text => 3; _ => 4}"
                    , testExpectSuccess
                        "testeq 1 $ 34.0 >- fn {_:?Integer => 1; _:?Rational => 2; _:?Text => 3; _ => 4}"
                    , testExpectSuccess
                        "testeq 2 $ 34.0 >- fn {_:?Rational => 2; _:?Integer => 1; _:?Text => 3; _ => 4}"
                    ]
                , tGroup
                    "List"
                    [ testExpectSuccess "testeq 2 $ [] >- fn {_ :: _ => 1; [] => 2}"
                    , testExpectSuccess "testeq 2 $ [] >- fn {_ :: _ => 1; _ => 2}"
                    , testExpectSuccess "testeq 2 $ [] >- fn {_:? List1 Integer => 1; _ => 2}"
                    , testExpectSuccess "testeq 1 $ [3,4] >- fn {_ :: _ => 1; [] => 2}"
                    , testExpectSuccess "testeq 1 $ [3,4] >- fn {_ :: _ => 1; _ => 2}"
                    , testExpectSuccess "testeq 1 $ [3,4] >- fn {_:? List1 Integer => 1; _ => 2}"
                    ]
                ]
            , tDecls
                [ "datatype T {T1 Text Number; T2; T3 Boolean; T4 (WholeModel (-Boolean,+Integer) -> Integer); T5 Text (Boolean -> Integer)}"
                ]
                $ tGroup
                    "datatype"
                    [ tGroup
                        "simple"
                        [ testExpectSuccess "pass"
                        , testExpectSuccess "let {t1 = T1.T \"hello\" 3} pass"
                        , testExpectSuccess "let {f = fn T1.T x _ => x} pass"
                        , testExpectSuccess "T2.T >- fn {T2.T => pass}"
                        , testExpectSuccess "T3.T True >- fn {T3.T True => pass}"
                        , testExpectSuccess "T1.T \"hello\" 3 >- fn {T1.T \"hello\" 3 => pass}"
                        , testExpectSuccess
                            "T1.T \"hello\" 3 >- fn {T2.T => fail \"T2.T\"; T1.T \"hello\" 2 => fail \"T1.T 2\"; T1.T \"hell\" 3 => fail \"T1.T hell\"; T1.T \"hello\" 3 => pass}"
                        , testExpectSuccess
                            "let {f : Boolean -> Integer = fn b => if b then 1 else 0} T5.T \"abcd\" f >- fn {T5.T _ ff => if ff True == 1 then pass else fail \"ff\"}"
                        , testExpectReject "let {datatype B {Mk a}} pass"
                        , testExpectSuccess "let {datatype P {}} pass"
                        , testExpectSuccess "let {datatype P {P1}} pass"
                        , testExpectSuccess "let {datatype P {P1;}} pass"
                        , testExpectSuccess "let {datatype P {P1 Integer}} pass"
                        , testExpectSuccess "let {datatype P {P1 Integer;}} pass"
                        , testExpectSuccess "let {datatype P {P1 Integer; P2 Text}} pass"
                        , testExpectSuccess "let {datatype P {P1 Integer; P2 Text;}} pass"
                        ]
                    , tGroup
                        "nominal"
                        [ testExpectSuccess "let {datatype P {P1}; f : P -> P = fn x => x} pass"
                        , testExpectReject "let {datatype P {P1}; datatype Q {}; f : P -> Q = fn x => x} pass"
                        , testExpectReject "let {datatype P {}; datatype Q {Q1}; f : P -> Q = fn x => x} pass"
                        , testExpectReject "let {datatype P {}; datatype Q {}; f : P -> Q = fn x => x} pass"
                        , testExpectReject "let {datatype P {P1}; datatype Q {Q1}; f : P -> Q = fn x => x} pass"
                        , testExpectReject
                            "let {datatype P {P1 Integer}; datatype Q {Q1 Integer}; f : P -> Q = fn x => x} pass"
                        ]
                    , tGroup
                        "contain-recursive"
                        [ tDecls
                            [ "datatype P {Mk (rec a, Maybe a)}"
                            , "let rec {fromR: (rec a, Maybe a) -> Integer = fn {Nothing => 0; Just a => 1 + fromR a}}"
                            , "let rec {toR: Integer -> (rec a, Maybe a) = fn {0 => Nothing; i => Just $ toR (i - 1)}}"
                            , "let rec {fromP: P -> Integer = fn {Mk.P Nothing => 0; Mk.P (Just a) => 1 + fromP (Mk.P a)}}"
                            , "let rec {toP: Integer -> P = fn {0 => Mk.P Nothing; i => toP (i - 1) >- fn Mk.P a => Mk.P $ Just a}}"
                            ]
                            $ tGroup
                                "Maybe"
                                [ testExpectSuccess "pass"
                                , testExpectSuccess "testeq 5 $ fromP $ Mk.P $ Just $ Just $ Just $ Just $ Just Nothing"
                                , testExpectSuccess "testeq 17 $ fromP $ toP 17"
                                , testExpectSuccess "testeq 17 $ fromP $ Mk.P $ toR 17"
                                , testExpectSuccess "testeq 17 $ fromR $ toP 17 >- fn Mk.P a => a"
                                ]
                        , tDecls
                            [ "datatype Q +t {Mk (rec a, Maybe (t *: a))}"
                            , "let rec {fromR: (rec a, Maybe (t *: a)) -> List t = fn {Nothing => []; Just (t,a) => t :: fromR a}}"
                            , "let rec {toR: List t -> (rec a, Maybe (t *: a)) = fn {[] => Nothing; t :: tt => Just (t, toR tt)}}"
                            , "let rec {fromQ: Q t -> List t = fn {Mk.Q Nothing => []; Mk.Q (Just (t,a)) => t :: fromQ (Mk.Q a)}}"
                            , "let rec {toQ: List t -> Q t = fn {[] => Mk.Q Nothing; t :: tt => Mk.Q $ Just (t, toQ tt >- fn Mk.Q a => a)}}"
                            ]
                            $ tGroup
                                "Pair"
                                [ testExpectSuccess "pass"
                                , testExpectSuccess "testeq [5,3,1] $ fromQ $ Mk.Q $ Just (5,Just (3,Just (1,Nothing)))"
                                , testExpectSuccess "testeq [8,12,27,45] $ fromQ $ toQ [8,12,27,45]"
                                , testExpectSuccess "testeq [8,12,27,45] $ fromQ $ Mk.Q $ toR [8,12,27,45]"
                                , testExpectSuccess "testeq [8,12,27,45] $ fromR $ toQ [8,12,27,45] >- fn Mk.Q a => a"
                                ]
                        ]
                    , tGroup
                        "recursive"
                        [ testExpectSuccess "let {datatype P {P1}} let {datatype Q {Q1 P}} pass"
                        , testExpectSuccess "let {datatype P {P1}; datatype Q {Q1 P}} pass"
                        , testExpectSuccess "let rec {datatype P {P1 Q}; datatype Q {}} pass"
                        , testExpectSuccess "let rec {datatype P {P1 Q}; datatype Q {Q1 P}} pass"
                        , testExpectSuccess "let rec {datatype P {P1 P}} pass"
                        , testExpectSuccess
                            "let rec {datatype P {P1 Q}; datatype Q {Q1 P}; f : P -> P = fn {P1.P q => q >- fn {Q1.Q p => p}}} pass"
                        , testExpectSuccess "let rec {datatype P {P1 Q}; datatype storable Q {Q1 !\"Q1\"}} pass"
                        , testExpectReject "let rec {datatype storable P {P1 Q}; datatype Q {Q1 !\"Q1\"}} pass"
                        , testExpectSuccess
                            "let rec {datatype P {P1 Q}; datatype Q {Q1 (Action Unit)}; pqpass = P1.P (Q1.Q pass)} pqpass >- fn {P1.P (Q1.Q p) => p}"
                        ]
                    , tGroup
                        "parameters"
                        [ tGroup
                            "variance"
                            [ testExpectSuccess "let {datatype B +a {Mk a}} pass"
                            , testExpectReject "let {datatype B -a {Mk a}} pass"
                            , testExpectSuccess "let {datatype B -a {Mk (a -> Boolean)}} pass"
                            , testExpectReject "let {datatype B +a {Mk (a -> Boolean)}} pass"
                            , testExpectSuccess "let {datatype B (-p,+q) {Mk (p -> q)}} pass"
                            , testExpectSuccess "let {datatype B (+q,-p) {Mk (p -> q)}} pass"
                            , testExpectReject "let {datatype B (-p,+q) {Mk (q -> p)}} pass"
                            , testExpectReject "let {datatype B (+q,-p) {Mk (q -> p)}} pass"
                            ]
                        , tGroup
                            "recursive"
                            [ testExpectSuccess "let rec {datatype R +a {Mk (R a)}} pass"
                            , testExpectSuccess "let rec {datatype R -a {Mk (R a)}} pass"
                            , testExpectSuccess
                                "let rec {datatype R1 +a {MkR1 (R2 a)}; datatype R2 +a {MkR2 (R1 a)}} pass"
                            , testExpectSuccess
                                "let rec {datatype R1 -a {MkR1 (R2 a)}; datatype R2 -a {MkR2 (R1 a)}} pass"
                            , testExpectSuccess
                                "let rec {datatype R1 +a {MkR1 (R2 a -> Integer)}; datatype R2 -a {MkR2 (R1 a -> Integer)}} pass"
                            ]
                        , tGroup
                            "conversion"
                            [ tDecls
                                [ "datatype D +a {Mk1 (List a); Mk2 (Maybe a)}"
                                , "showD: D Showable -> Text = fn {Mk1.D aa => show aa; Mk2.D ma => show ma}"
                                , "di: D Integer = Mk1.D [576,469,12]"
                                , "sdi: Text = showD di"
                                ]
                                $ testExpectSuccess "if sdi == \"[576,469,12]\" then pass else fail sdi"
                            , tDecls
                                [ "datatype D -a {Mk1 (a -> Integer); Mk2 (a -> a -> Text)}"
                                , "dShow: D Number = Mk2.D $ fn a, b => show a <>.Text \",\" <>.Text show b"
                                , "di: D Integer = dShow"
                                , "showD: a -> D a -> Text = fn a => fn {Mk1.D ai => show $ ai a; Mk2.D aat => aat a a}"
                                , "sd: Text = showD 356 di"
                                ]
                                $ testExpectSuccess "if sd == \"356,356\" then pass else fail sd"
                            , tDecls
                                [ "let rec {datatype RList +a {Mk (Maybe (a *: RList a))}}"
                                , "let rec {showRList: RList Showable -> Text = fn Mk.RList rl => rl >- fn {Nothing => \"\"; Just (a,rla) => show a <>.Text \";\" <>.Text showRList rla}}"
                                , "rlisti: RList Integer = Mk.RList $ Just (45,Mk.RList $ Just (72, Mk.RList $ Just (18,Mk.RList Nothing)))"
                                , "rlists: RList Showable = rlisti"
                                , "sd: Text = showRList rlists"
                                ]
                                $ testExpectSuccess "if sd == \"45;72;18;\" then pass else fail sd"
                            ]
                        , tGroup
                            "doubled"
                            [ tDecls ["datatype F a {Mk (a -> a)}", "unF = fn Mk.F f => f"]
                                $ tGroup
                                    "plain"
                                    [ testExpectSuccess "pass"
                                    , testExpectSuccess "let {f = Mk.F id} testeq 3 $ unF f 3"
                                    , testExpectSuccess "let {f = Mk.F show} testeq \"3\" $ unF f 3"
                                    ]
                            , tDecls ["datatype F a {Mk {mf: a -> a}}", "unF = fn Mk.F => mf"]
                                $ tGroup
                                    "record"
                                    [ testExpectSuccess "pass"
                                    , testExpectSuccess "let {f = Mk.F {mf = id}} testeq 3 $ unF f 3"
                                    , testExpectSuccess "let {f = Mk.F {mf = show}} testeq \"3\" $ unF f 3"
                                    ]
                            ]
                        ]
                    , tGroup
                        "subtype"
                        [ tDecls ["datatype D1 {Mk Integer; subtype datatype D2 {Mk; subtype datatype D3 {Mk Boolean;};};}"]
                            $ tGroup
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
                            "let rec {datatype D1 {Mk D1 D2 D3.D2; subtype datatype D2 {Mk D1 D2 D3; subtype datatype D3 {P D1 D2 D3; Q;}}}}"
                            $ tGroup
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
                                    [ testExpectSuccess $ isOfType "fn {Mk.D1 _ _ _ => ()}" "D1 -> Unit"
                                    , testExpectSuccess $ isOfType "fn {Mk.D2.D1 _ _ _ => ()}" "D1 -> Unit"
                                    , testExpectSuccess $ isOfType "fn {P.D3.D2.D1 _ _ _ => ()}" "D1 -> Unit"
                                    , testExpectSuccess $ isOfType "fn {Q.D3.D2.D1 => ()}" "D1 -> Unit"
                                    ]
                                ]
                        , testExpectSuccess "let rec {datatype L {LNil; subtype datatype L1 {LCons Unit L}}} pass"
                        , testExpectSuccess "let rec {datatype L +a {LNil; subtype datatype L1 {LCons a (L a)}}} pass"
                        ]
                    , tGroup
                        "record-constructor"
                        [ tGroup
                            "rank-1"
                            [ tDecls
                                [ "datatype R {Mk {di: Integer}}"
                                , "mkR1: Integer -> R = fn x => Mk.R {di = x}"
                                , "mkR2: Integer -> R = fn di => Mk.R"
                                ]
                                $ tGroup
                                    "one"
                                    [ testExpectSuccess "pass"
                                    , testExpectSuccess "testeq 21 $ (fn Mk.R => di) (mkR1 21)"
                                    , testExpectSuccess "testeq 21 $ (fn Mk.R => di) (mkR2 21)"
                                    , testExpectSuccess "testeq 22 $ (fn {Mk.R => di}) (mkR1 22)"
                                    , testExpectSuccess "testeq 22 $ (fn {Mk.R => di}) (mkR2 22)"
                                    , testExpectSuccess "testeq 23 $ let {Mk.R = mkR1 23} 23"
                                    , testExpectSuccess "testeq 23 $ let {Mk.R = mkR2 23} 23"
                                    , testExpectSuccess "testeq 24 $ let {Mk.R = mkR1 24} di"
                                    , testExpectSuccess "testeq 24 $ let {Mk.R = mkR2 24} di"
                                    , testExpectSuccess "testeq 25 $ let {Just di = Just 25} di"
                                    , testExpectSuccess "let {g: R -> Integer = fn Mk.R => di;} testeq 26 $ g $ mkR1 26"
                                    , testExpectReject "testeq 27 $ let {Mk.R as A = mkR1 27} di"
                                    , testExpectSuccess "testeq 28 $ let {Mk.R as A = mkR1 28} di.A"
                                    ]
                            , tDecls ["datatype R {Mk {di: Integer; dt: Text}}"]
                                $ tGroup
                                    "two"
                                    [ testExpectSuccess "pass"
                                    , testExpectSuccess
                                        "let {f: Integer -> R = fn di => let {dt = \"t\"} Mk.R; g: R -> Integer = fn Mk.R => di;} testeq 17 $ g $ f 17"
                                    , testExpectSuccess
                                        "let {f: Integer -> R = fn di => let {dt = \"t\"} Mk.R; g: R -> Text = fn Mk.R => dt;} testeq \"t\" $ g $ f 17"
                                    ]
                            ]
                        , tDecls
                            [ "datatype UU {Mk {r1: Unit; r2: Unit;}}"
                            , "testr = fn f1, a1, f2, a2 => let {r1 = f1 a1; r2 = f2 a2;} Mk.UU"
                            , "testrs = fn f1, a1, f2, a2 => let {r1:Unit = f1 a1; r2:Unit = f2 a2;} Mk.UU"
                            , "testps = fn f1, a1, f2, a2 => let {r1:Unit = f1 a1; r2:Unit = f2 a2;} (r1,r2)"
                            ]
                            $ tGroup
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
                            [ "datatype R {Mk {df: (a -> a) -> a -> a}}"
                            , "twice = fn f, x => f (f x)"
                            , "addone: Integer -> Integer = fn x => x + 1"
                            ]
                            $ tGroup
                                "rank-2"
                                [ testExpectSuccess "pass"
                                , testExpectSuccess "let {r:R = Mk.R {df = twice}} r >- fn Mk.R => testeq 9 $ df addone 7"
                                , testExpectSuccess
                                    "let {r:R = let {df: (b -> b) -> b -> b = twice} Mk.R} r >- fn Mk.R => testeq 9 $ df addone 7"
                                , testExpectReject
                                    "let {r:R = let {df: (Integer -> Integer) -> Integer -> Integer = twice} Mk.R} r >- fn Mk.R => testeq 9 $ df addone 7"
                                ]
                        , tDecls
                            [ "datatype R +v {Mk {df: a -> Maybe (a *: v)}}"
                            , "ff: a -> Maybe (a *: Integer) = fn x => Just (x,45)"
                            , "r = Mk.R {df = ff}"
                            ]
                            $ tGroup
                                "sub-var"
                                [ testExpectSuccess "pass"
                                , testExpectSuccess $ isOfType "r" "R Integer"
                                , testExpectReject $ isOfType "r" "R Text"
                                ]
                        , tGroup
                            "inversion"
                            [ tDecls
                                [ "datatype W +a {Mk a}"
                                , "wrap: a -> W a = Mk.W"
                                , "unwrap: W a -> a = fn (Mk.W val) => val"
                                ]
                                $ tGroup
                                    "plain"
                                    [testExpectSuccess "pass", testExpectSuccess "testeq 374 $ unwrap $ wrap 374"]
                            , tGroup
                                "record"
                                [ tDecls
                                    [ "datatype W +p {Mk {val: p}}"
                                    , "wrap: x -> W x = fn val => Mk.W"
                                    , "unwrap: W y -> y = fn Mk.W => val"
                                    ]
                                    $ tGroup
                                        "id"
                                        [testExpectSuccess "pass", testExpectSuccess "testeq 374 $ unwrap $ wrap 374"]
                                , tDecls
                                    [ "datatype W +p {Mk {val: p}}"
                                    , "wrap: x -> W x = fn v => let {val = v} Mk.W"
                                    , "unwrap: W y -> y = fn Mk.W => val"
                                    ]
                                    $ tGroup
                                        "let"
                                        [testExpectSuccess "pass", testExpectSuccess "testeq 374 $ unwrap $ wrap 374"]
                                , tDecls
                                    [ "datatype W +p {Mk {val: Maybe p}}"
                                    , "wrap: Maybe x -> W x = fn val => Mk.W"
                                    , "unwrap: W y -> Maybe y = fn Mk.W => val"
                                    ]
                                    $ tGroup
                                        "Maybe"
                                        [ testExpectSuccess "pass"
                                        , testExpectSuccess "testeq (Just 374) $ unwrap $ wrap $ Just 374"
                                        ]
                                , tDecls
                                    [ "datatype R +p {Mk {val: q -> (q *: p)}}"
                                    , "f = fn x => let {val = fn y => (y,x)} Mk.R"
                                    ]
                                    $ tGroup
                                        "infer-var"
                                        [testExpectSuccess "pass", testExpectSuccess $ isOfType "f" "a -> R a"]
                                , tDecls
                                    [ "datatype R +p {Mk {val: q -> (q *: p)}}"
                                    , "f = fn x => let {val = fn y => (y,x+1)} Mk.R"
                                    ]
                                    $ tGroup
                                        "infer-Integer"
                                        [ testExpectSuccess "pass"
                                        , testExpectSuccess $ isOfType "f" "Integer -> R Integer"
                                        ]
                                ]
                            ]
                        , tDecls
                            [ "datatype R +a {Mk {val: List a}}"
                            , "mkR: List a -> R a = fn val => Mk.R"
                            , "rShow: R Showable -> Text = fn Mk.R => show val"
                            ]
                            $ tGroup
                                "map"
                                [ testExpectSuccess "pass"
                                , testExpectSuccess
                                    "let {r1: R Integer = mkR []; r2: R Showable = r1} testeq \"[]\" $ rShow r2"
                                , testExpectSuccess
                                    "let {r1: R Integer = mkR [57]; r2: R Showable = r1} testeq \"[57]\" $ rShow r2"
                                , testExpectSuccess
                                    "let {r1: R Integer = mkR [12, 10, 57]; r2: R Showable = r1} testeq \"[12,10,57]\" $ rShow r2"
                                ]
                        , tDecls
                            [ "datatype Rec +a {Mk {rval: rec r, Maybe (a *: r)}}"
                            , "rec0: Rec a = let {rval = Nothing} Mk.Rec"
                            , "rec1: a -> Rec a = fn x0 => let {rval = Just (x0,Nothing)} Mk.Rec"
                            , "rec3: a -> a -> a -> Rec a = fn x0, x1, x2 => let {rval = Just (x0,Just (x1,Just (x2,Nothing)))} Mk.Rec"
                            , "let rec {rShow: (rec r, Maybe (Showable *: r)) -> Text = fn {Nothing => \"\"; Just (a,r) => show a <>.Text \",\" <>.Text rShow r}}"
                            , "recShow: Rec Showable -> Text = fn Mk.Rec => rShow rval"
                            ]
                            $ tGroup
                                "recursive"
                                [ testExpectSuccess "pass"
                                , testExpectSuccess
                                    "let {r1: Rec Integer = rec0; r2: Rec Showable = r1} testeq \"\" $ recShow r2"
                                , testExpectSuccess
                                    "let {r1: Rec Integer = rec1 57; r2: Rec Showable = r1} testeq \"57,\" $ recShow r2"
                                , testExpectSuccess
                                    "let {r1: Rec Integer = rec3 12 10 57; r2: Rec Showable = r1} testeq \"12,10,57,\" $ recShow r2"
                                ]
                        , tGroup
                            "override"
                            [ tGroup "type" $ let
                                testOverride :: Text -> Text -> Text
                                testOverride ta tb =
                                    "let {datatype A {Mk {val: "
                                        <> ta
                                        <> "}}; datatype B <: A {Mk {Mk.A; val: "
                                        <> tb
                                        <> "}}} pass"
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
                            , tDecls ["datatype A {Mk {val: Integer}}", "datatype B {Mk {val: Rational}}"]
                                $ tGroup
                                    "multiple"
                                    [ testExpectSuccess "pass"
                                    , testExpectReject "let {datatype C <: A & B {Mk {Mk.A; Mk.B}}} pass"
                                    , testExpectReject "let {datatype C <: A & B {Mk {Mk.A; Mk.B; val: Rational}}} pass"
                                    , testExpectSuccess "let {datatype C <: A & B {Mk {Mk.A; Mk.B; val: Integer}}} pass"
                                    ]
                            ]
                        , tDecls ["datatype R {Mk {u: Unit}}"] $ let
                            testExpr t = testExpectSuccess $ "let {f = " <> t <> "} seq f pass"
                            in tGroup
                                "issue-199"
                                [ testExpectSuccess "pass"
                                , testExpr "fn Mk.R => u"
                                , testExpr "fn a => let {Mk.R = a} u"
                                , testExpr "fn Mk.R => let {Mk.R = Mk.R} u"
                                , testExpr "fn Mk.R => fn Mk.R => u"
                                , testExpr "fn a => let {Mk.R = a; Mk.R = a;} u"
                                ]
                        , tGroup
                            "supertype"
                            [ tDecls
                                [ "datatype S {Mk {sval: Text}}"
                                , "mkS: Text -> S = fn sval => Mk.S"
                                , "unS: S -> Text = fn Mk.S => sval"
                                , "datatype R <: S {Mk {Mk.S; rval: Integer}}"
                                , "mkR: Text *: Integer -> R = fn (sval,rval) => Mk.R"
                                , "unR: R -> Text *: Integer = fn Mk.R => (sval,rval)"
                                ]
                                $ tGroup
                                    "simple"
                                    [ testExpectSuccess "pass"
                                    , testExpectSuccess "testeq (\"textx\", 56) $ unR $ mkR (\"textx\", 56)"
                                    , subtypeTest False SRSingle "R" "S"
                                    , subtypeTest False SRNot "S" "R"
                                    ]
                            , tDecls
                                [ "datatype S {Mk {val: Rational}}"
                                , "mkS: Rational -> S = fn val => Mk.S"
                                , "unS: S -> Rational = fn Mk.S => val"
                                , "datatype R <: S {Mk {Mk.S; val: Integer}}"
                                , "mkR: Integer -> R = fn val => Mk.R"
                                , "unR: R -> Integer = fn Mk.R => val"
                                ]
                                $ tGroup
                                    "refinement"
                                    [ testExpectSuccess "pass"
                                    , testExpectSuccess "testeq 74 $ unS $ mkR 74"
                                    , subtypeTest False SRSingle "R" "S"
                                    , subtypeTest False SRNot "S" "R"
                                    ]
                            , tDecls ["datatype S1 {Mk {val: Rational}}", "datatype S2 {Mk {val: Rational}}"]
                                $ tGroup
                                    "match-type"
                                    [ testExpectSuccess "let {datatype R <: S1 {Mk {Mk.S1}}} pass"
                                    , testExpectReject "let {datatype R <: S1 {Mk {Mk.S2}}} pass"
                                    , testExpectSuccess "let {datatype R {Mk {}}} pass"
                                    , testExpectReject "let {datatype R <: S1 {Mk {}}} pass"
                                    , testExpectReject "let {datatype R {Mk {Mk.S1}}} pass"
                                    , testExpectSuccess
                                        "let {datatype R <: S1 & S2 {Mk {Mk.S1; Mk.S2; val: Rational}}} pass"
                                    , testExpectSuccess "let {datatype R <: S1 & S2 {Mk {Mk.S1; Mk.S2; val: Integer}}} pass"
                                    , testExpectReject "let {datatype R <: S1 & S2 {Mk {Mk.S1}}} pass"
                                    , testExpectReject "let {datatype R <: S1 & S2 {Mk {Mk.S2}}} pass"
                                    ]
                            , tGroup
                                "multiple-supertype"
                                [ tDecls
                                    [ "datatype A {Mk {ma: Integer}}"
                                    , "datatype B {Mk {mb: Text}}"
                                    , "datatype C <: A & B {Mk {Mk.A; Mk.B}}"
                                    , "c = let {ma = 75; mb = \"ttmb\"} Mk.C"
                                    ]
                                    $ tGroup
                                        "single-constructor"
                                        [ testExpectSuccess "pass"
                                        , testExpectSuccess "testeq (\"ttmb\",75) $ c >- fn Mk.B@Mk.A => (mb,ma)"
                                        , testExpectSuccess "testeq (\"ttmb\",75) $ c >- fn Mk.C => (mb,ma)"
                                        ]
                                , tDecls
                                    [ "datatype A {Mk1 {ma1: Integer}; Mk2 {ma2: Text};}"
                                    , "datatype B {Mk1 {mb1: Integer}; Mk2 {mb2: Text};}"
                                    , "datatype C <: A & B {Mk {Mk2.A; Mk1.B}}"
                                    , "c = let {mb1 = 77; ma2 = \"ttma1\"} Mk.C"
                                    ]
                                    $ tGroup
                                        "multiple-constructor"
                                        [ testExpectSuccess "pass"
                                        , testExpectSuccess "testeq (\"ttma1\",77) $ c >- fn Mk2.A@Mk1.B => (ma2,mb1)"
                                        , testExpectSuccess "testeq (\"ttma1\",77) $ c >- fn Mk.C => (ma2,mb1)"
                                        ]
                                , tDecls
                                    [ "datatype A {Mk1 {ma1: Integer}; Mk2 {ma2: Text};}"
                                    , "datatype B <: A {Mk1 {Mk1.A; mb1: Integer}; Mk2 {Mk2.A; mb2: Text};}"
                                    , "datatype C <: A {Mk1 {Mk1.A; mc1: Integer}; Mk2 {Mk2.A; mc2: Text};}"
                                    ]
                                    $ tGroup
                                        "diamond"
                                        [ testExpectSuccess "pass"
                                        , tDecls
                                            [ "datatype D <: B & C {Mk {Mk1.B; Mk1.C;}}"
                                            , "d = let {ma1 = 58; mb1 = 59; mc1 = 60} Mk.D"
                                            ]
                                            $ tGroup
                                                "consistent"
                                                [ testExpectSuccess "pass"
                                                , testExpectSuccess "testeq (58,59,60) $ d >- fn Mk.D => (ma1,mb1,mc1)"
                                                , testExpectSuccess
                                                    "testeq (58,59,60) $ d >- fn Mk1.B@Mk1.C => (ma1,mb1,mc1)"
                                                ]
                                        , tDecls ["datatype D <: B & C {Mk {Mk1.B; Mk2.C;}}"]
                                            $ tGroup "inconsistent" [testExpectReject "pass"]
                                        ]
                                ]
                            ]
                        , tGroup
                            "default"
                            [ tDecls ["datatype A {Mk {ma: Integer = 754}}"]
                                $ tGroup
                                    "simple"
                                    [ testExpectSuccess "pass"
                                    , testExpectSuccess "testeq 42 $ Mk.A {ma = 42} >- fn Mk.A => ma"
                                    , testExpectSuccess "testeq 42 $ (let {ma = 42} Mk.A) >- fn Mk.A => ma"
                                    , testExpectSuccess "testeq 754 $ Mk.A {} >- fn Mk.A => ma"
                                    , testExpectSuccess "testeq 754 $ Mk.A >- fn Mk.A => ma"
                                    ]
                            , tDecls
                                [ "datatype A {Mk1 {ma1: Integer = 755}; Mk2 {ma2: Text};}"
                                , "datatype B <: A {Mk1 {Mk1.A; mb1: Integer}; Mk2 {Mk2.A; mb2: Text};}"
                                , "datatype C <: A {Mk1 {Mk1.A; mc1: Integer}; Mk2 {Mk2.A; mc2: Text};}"
                                , "datatype D <: B & C {Mk {Mk1.B; Mk1.C;}}"
                                ]
                                $ tGroup
                                    "diamond"
                                    [ testExpectSuccess "pass"
                                    , testExpectSuccess "testeq 43 $ Mk.D {ma1 = 43; mb1 = 44; mc1 = 45} >- fn Mk.D => ma1"
                                    , testExpectSuccess
                                        "testeq 43 $ (let {ma1 = 43; mb1 = 44; mc1 = 45} Mk.D) >- fn Mk.D => ma1"
                                    , testExpectSuccess "testeq 755 $ Mk.D {mb1 = 46; mc1 = 47} >- fn Mk.D => ma1"
                                    , testExpectSuccess "testeq 755 $ (let {mb1 = 46; mc1 = 47} Mk.D) >- fn Mk.D => ma1"
                                    ]
                            ]
                        ]
                    ]
            , tGroup
                "subtype-decl"
                [ tDecls ["datatype T {T1 Integer}", "unT1 = fn T1.T x => x"]
                    $ tGroup
                        "simple"
                        [ tDecls ["subtype Integer <: T = T1.T"]
                            $ tGroup
                                "Integer <: T"
                                [ testExpectSuccess "pass"
                                , subtypeTest False SRSingle "Integer" "T"
                                , subtypeTest False SRNot "T" "Integer"
                                ]
                        , tDecls ["subtype T <: Integer = unT1"]
                            $ tGroup
                                "T <: Integer"
                                [ testExpectSuccess "pass"
                                , subtypeTest False SRNot "Integer" "T"
                                , subtypeTest False SRSingle "T" "Integer"
                                ]
                        ]
                , tDecls ["datatype T +a {T1 (Maybe a)}", "unT1 = fn T1.T x => x"]
                    $ tGroup
                        "parameter"
                        [ tDecls ["subtype Maybe Integer <: T Integer = T1.T"]
                            $ tGroup
                                "plain"
                                [ testExpectSuccess "pass"
                                , subtypeTest False SRSingle "Maybe Integer" "T Integer"
                                , testExpectSuccess "testeq (Just 3) $ unT1 $ Just 3"
                                ]
                        , tDecls ["subtype Maybe a <: T a = T1.T"]
                            $ tGroup
                                "tyvar"
                                [ testExpectSuccess "pass"
                                , subtypeTest False SRSingle "Maybe Integer" "T Integer"
                                , testExpectSuccess "testeq (Just 3) $ unT1 $ Just 3"
                                ]
                        ]
                , tDecls ["datatype T +a {T1 (Maybe a)}", "unT1 = fn T1.T x => x"]
                    $ tGroup
                        "dependent"
                        [ testExpectSuccess
                            $ "let {x = 17; f = let {subtype Unit <: T Integer = fn () => T1.T (Just x)} unT1 ()} testeq (Just 17) f"
                        , testExpectSuccess
                            $ "let rec {f = let {subtype Unit <: T Integer = fn () => T1.T (Just x)} unT1 (); x = 17} testeq (Just 17) f"
                        , testExpectSuccess
                            $ "let {f = fn x => let {subtype Unit <: T Integer = fn () => T1.T (Just x)} unT1 ()} testeq (Just 17) $ f 17"
                        , testExpectSuccess
                            $ "let {f = fn x => let {y = x; subtype Unit <: T Integer = fn () => T1.T (Just y)} unT1 ()} testeq (Just 17) $ f 17"
                        ]
                ]
            , tDecls ["datatype storable T {T1 Text Number !\"T.T1\"; T2 !\"T.T2\"; T3 Boolean !\"T.T3\"}"]
                $ tGroup
                    "datatype-storable"
                    [ testExpectSuccess "pass"
                    , testExpectSuccess "let {f: T -> Entity = fn x => x} pass"
                    , testExpectSuccess "let {t1 = T1.T \"hello\" 3} pass"
                    , testExpectSuccess "let {f = fn T1.T x _ => x} pass"
                    , testExpectSuccess "T1.T \"hello\" 3 >- fn {T1.T \"hello\" 3 => pass}"
                    , testExpectSuccess
                        "T1.T \"hello\" 3 >- fn {T2.T => fail \"T2\"; T1.T \"hello\" 2 => fail \"T1.T 2\"; T1.T \"hell\" 3 => fail \"T1.T hell\"; T1.T \"hello\" 3 => pass}"
                    , testExpectSuccess "let {datatype storable P {}} pass"
                    , testExpectSuccess "let {datatype storable P {P1 !\"P1\"}} pass"
                    , testExpectSuccess "let {datatype storable P {P1 !\"P1\";}} pass"
                    , testExpectSuccess "let {datatype storable P {P1 Integer !\"P1\"}} pass"
                    , testExpectSuccess "let {datatype storable P {P1 Integer !\"P1\";}} pass"
                    , testExpectSuccess "let {datatype storable P {P1 Integer !\"P1\"; P2 Text !\"P2\"}} pass"
                    , testExpectSuccess "let {datatype storable P {P1 Integer !\"P1\"; P2 Text !\"P2\";}} pass"
                    , tGroup
                        "nominal"
                        [ testExpectSuccess "let {datatype storable P {P1 !\"P1\"}; f : P -> P = fn x => x} pass"
                        , testExpectReject
                            "let {datatype storable P {P1 !\"P1\"}; datatype storable Q {}; f : P -> Q = fn x => x} pass"
                        , testExpectReject
                            "let {datatype storable P {}; datatype storable Q {Q1 !\"Q1\"}; f : P -> Q = fn x => x} pass"
                        , testExpectReject
                            "let {datatype storable P {}; datatype storable Q {}; f : P -> Q = fn x => x} pass"
                        , testExpectReject
                            "let {datatype storable P {P1 !\"P1\"}; datatype storable Q {Q1 !\"Q1\"}; f : P -> Q = fn x => x} pass"
                        , testExpectReject
                            "let {datatype storable P {P1 Integer !\"P1\"}; datatype storable Q {Q1 Integer !\"Q1\"}; f : P -> Q = fn x => x} pass"
                        ]
                    , tGroup
                        "parameters"
                        [ testExpectSuccess
                            "let {datatype storable P +a {P1 !\"P1\"}; f : P Integer -> P Integer = fn x => x} pass"
                        , testExpectReject
                            "let {datatype storable P -a {P1 !\"P1\"}; f : P Integer -> P Integer = fn x => x} pass"
                        , testExpectReject
                            "let {datatype storable P a {P1 !\"P1\"}; f : P Integer -> P Integer = fn x => x} pass"
                        , testExpectSuccess
                            "let {datatype storable P +a {P1 a !\"P1\"}; f : P Integer -> P Integer = fn x => x} pass"
                        , testExpectSuccess
                            "let {datatype storable P +a {P1 a !\"P1\"}; f : P Integer -> Integer= fn P1.P x => x} pass"
                        , testExpectSuccess "let {datatype storable P {P1 !\"P1\"}; f : P -> Entity = fn x => x} pass"
                        , testExpectSuccess
                            "let {datatype storable P +a {P1 a !\"P1\"}; f : P Entity -> Entity = fn x => x} pass"
                        , testExpectSuccess
                            "let {datatype storable P +a +b {P1 a b !\"P1\"}; f : P Entity Entity -> Entity = fn x => x} pass"
                        ]
                    , tGroup
                        "recursive"
                        [ testExpectSuccess
                            "let {datatype storable P {P1 !\"P1\"}} let {datatype storable Q {Q1 P !\"Q1\"}} pass"
                        , testExpectSuccess
                            "let {datatype storable P {P1 !\"P1\"}; datatype storable Q {Q1 P !\"Q1\"}} pass"
                        , testExpectSuccess
                            "let rec {datatype storable P {P1 !\"P1\"}; datatype storable Q {Q1 P !\"Q1\"}} pass"
                        , testExpectSuccess "let rec {datatype storable P {P1 Q !\"P1\"}; datatype storable Q {}} pass"
                        , testExpectSuccess
                            "let rec {datatype storable P {P1 Q !\"P1\"}; datatype storable Q {Q1 P !\"Q1\"}} pass"
                        , testExpectSuccess "let rec {datatype storable P {P1 P !\"P1\"}} pass"
                        , testExpectSuccess "let rec {datatype storable P +a {P1 (P (a *: a)) !\"P1\"}} pass"
                        , tDecls
                            [ "let rec {datatype storable Nat {Z !\"Z\"; S Nat !\"S\"}}"
                            , "let rec {natToInteger: Nat -> Integer = fn {Z.Nat => 0; S.Nat n => 1 + natToInteger n}}"
                            ]
                            $ tGroup
                                "Nat"
                                [ testExpectSuccess "pass"
                                , testExpectSuccess "testeq 0 $ natToInteger Z.Nat"
                                , testExpectSuccess "testeq 1 $ natToInteger $ S.Nat Z.Nat"
                                , testExpectSuccess "testneq 0 Z.Nat"
                                , testExpectSuccess "testeq Z.Nat Z.Nat"
                                ]
                        , tDecls
                            [ "let rec {datatype storable L +a {Nil !\"Nil\"; Cons a (L a) !\"Cons\"}}"
                            , "let rec {listToL: List a -> L a = fn {[] => Nil.L; x::xs => Cons.L x (listToL xs)}}"
                            , "let rec {lToList: L a -> List a = fn {Nil.L => []; Cons.L x xs => x :: lToList xs}}"
                            ]
                            $ tGroup
                                "list"
                                [ testExpectSuccess "pass"
                                , testExpectSuccess "let {l = listToL [1,2,3]} pass"
                                , testExpectSuccess "let {l = listToL [1,2,3]} testeq (lToList l) [1,2,3]"
                                , testExpectSuccess "testeq (lToList $ listToL [1,2,3]) [1,2,3]"
                                , testExpectSuccess "testneq 0 Nil.L"
                                , testExpectSuccess "testeq Nil.L Nil.L"
                                , testExpectSuccess "testeq (Cons.L 1 Nil.L) (Cons.L 1 Nil.L)"
                                , testExpectSuccess
                                    "testeq (Cons.L 1 (Cons.L 2 (Cons.L 3 Nil.L))) (Cons.L 1 (Cons.L 2 (Cons.L 3 Nil.L)))"
                                , testExpectSuccess "testeq (listToL [1,2,3]) (Cons.L 1 (Cons.L 2 (Cons.L 3 Nil.L)))"
                                , testExpectSuccess "testeq (lToList $ Cons.L 1 $ Cons.L 2 $ Cons.L 3 Nil.L) [1,2,3]"
                                ]
                        ]
                    ]
            , tGroup
                "type-escape"
                [ testExpectSuccess
                    "let {entitytype T; t = let {} !{point.OpenEntity @T !\"t\"}; f = let {f : T -> Action Unit = fn _ => pass} f;} f t"
                , testExpectReject
                    "let {entitytype T1; entitytype T2; t = let {} !{point.OpenEntity @T1 !\"t\"}; f = let {f : T2 -> Action Unit = fn _ => pass} f;} f t"
                , testExpectReject
                    "let {t = let {entitytype T} !{point.OpenEntity @T !\"t\"}; f = let {entitytype T; f : T -> Action Unit = fn _ => pass} f;} f t"
                , testExpectReject
                    "let {t = let {entitytype T1} !{point.OpenEntity @T1 !\"t\"}; f = let {entitytype T2; f : T2 -> Action Unit = fn _ => pass} f;} f t"
                ]
            , tGroup
                "general-subtype"
                [ testExpectReject "let {subtype Unit <: Unit = fn _ => ()} pass"
                , testExpectReject "let {subtype Integer <: Unit = fn _ => ()} pass"
                , tDecls ["entitytype P", "entitytype Q"]
                    $ tGroup
                        "entitytype"
                        [ testExpectSuccess "let {subtype P <: P} pass"
                        , testExpectSuccess "let {subtype P <: Q} pass"
                        , testExpectReject "let {subtype P <: P = fn x => x} pass"
                        , testExpectReject "let {subtype P <: Q = fn _ => error \"\"} pass"
                        ]
                , tDecls ["datatype P {Mk Number}"]
                    $ tGroup
                        "datatype"
                        [ testExpectSuccess "let {subtype Number <: P = Mk.P} pass"
                        , testExpectReject "let {subtype Number <: P = Mk.P; subtype Number <: P = Mk.P} pass"
                        , testExpectReject "let {subtype Number <: P = Mk.P; subtype Integer <: P = Mk.P} pass"
                        ]
                , tDecls
                    [ "datatype A {Mk Number}"
                    , "datatype B {Mk Number}"
                    , "datatype C {Mk Number}"
                    , "datatype D {Mk Number}"
                    , "subtype A <: B = fn Mk.A x => Mk.B x"
                    , "subtype C <: D = fn Mk.C x => Mk.D x"
                    , "subtype A <: D = fn Mk.A x => Mk.D x"
                    ]
                    $ tGroup
                        "verify"
                        [ testExpectSuccess "pass"
                        , testExpectReject "let {subtype B <: B = fn Mk.B x => Mk.B x} pass"
                        , testExpectReject "let {subtype B <: C = fn Mk.B x => Mk.C x} pass"
                        ]
                , tDecls ["datatype A {Mk Number}", "datatype B {Mk Number}"]
                    $ tGroup
                        "trustme"
                        [ testExpectSuccess "pass"
                        , testExpectSuccess "let {subtype A <: B = fn Mk.A x => Mk.B x} pass"
                        , testExpectReject
                            "let {subtype A <: B = fn Mk.A x => Mk.B x; subtype A <: B = fn Mk.A x => Mk.B x} pass"
                        , testExpectSuccess
                            "let {subtype A <: B = fn Mk.A x => Mk.B x; subtype trustme A <: B = fn Mk.A x => Mk.B x} pass"
                        , testExpectSuccess
                            "let {subtype trustme A <: B = fn Mk.A x => Mk.B x; subtype A <: B = fn Mk.A x => Mk.B x} pass"
                        ]
                , tDecls ["datatype A +x {Mk x}", "datatype B +x {Mk x}", "datatype C {Mk}"]
                    $ tGroup
                        "preferred"
                        [ testExpectSuccess "pass"
                        , testExpectSuccess "let {subtype trustme A Number <: B Number = fn Mk.A x => Mk.B x} pass"
                        , tModify (testTreeOne "1")
                            $ tDecls
                                [ "subtype trustme A Number <: B Number = fn Mk.A x => Mk.B x"
                                , "subtype trustme B Number <: C = fn Mk.B _ => Mk.C"
                                , "subtype trustme A Any <: C = fn Mk.A _ => Mk.C"
                                ]
                            $ subtypeTest False SRSingle "A Unit" "C"
                        , tModify (testTreeOne "2")
                            $ tDecls
                                [ "subtype trustme A Any <: C = fn Mk.A _ => Mk.C"
                                , "subtype trustme A Number <: B Number = fn Mk.A x => Mk.B x"
                                , "subtype trustme B Number <: C = fn Mk.B _ => Mk.C"
                                ]
                            $ subtypeTest False SRSingle "A Unit" "C"
                        , tModify (testTreeOne "3")
                            $ tDecls
                                [ "subtype trustme A a <: B a = fn Mk.A x => Mk.B x"
                                , "subtype trustme B Any <: C = fn Mk.B _ => Mk.C"
                                , "subtype trustme A Number <: C = fn Mk.A _ => Mk.C"
                                ]
                            $ subtypeTest False SRSingle "A Unit" "C"
                        , tModify (testTreeOne "4")
                            $ tDecls
                                [ "subtype trustme A Number <: C = fn Mk.A _ => Mk.C"
                                , "subtype trustme A a <: B a = fn Mk.A x => Mk.B x"
                                , "subtype trustme B Any <: C = fn Mk.B _ => Mk.C"
                                ]
                            $ subtypeTest False SRSingle "A Unit" "C"
                        , tGroup
                            "order"
                            [ tGroup
                                "var"
                                [ tDecls
                                    [ "subtype trustme A a <: B a = fn Mk.A x => Mk.B x"
                                    , "subtype trustme A Number <: B Number = fn Mk.A x => Mk.B x"
                                    ]
                                    $ subtypeTest False SRSingle "A Unit" "B Unit"
                                , tDecls
                                    [ "subtype trustme A Number <: B Number = fn Mk.A x => Mk.B x"
                                    , "subtype trustme A a <: B a = fn Mk.A x => Mk.B x"
                                    ]
                                    $ subtypeTest False SRSingle "A Unit" "B Unit"
                                ]
                            , tGroup
                                "incoherent"
                                [ tDecls
                                    [ "subtype trustme A Integer <: B Integer = fn Mk.A x => Mk.B x"
                                    , "subtype trustme A Number <: B Number = fn Mk.A x => Mk.B x"
                                    ]
                                    $ tGroup
                                        "1"
                                        [ subtypeTest False SRNot "A Number" "B Number"
                                        , subtypeTest False SRNot "A Integer" "B Integer"
                                        ]
                                , tDecls
                                    [ "subtype trustme A Number <: B Number = fn Mk.A x => Mk.B x"
                                    , "subtype trustme A Integer <: B Integer = fn Mk.A x => Mk.B x"
                                    ]
                                    $ tGroup
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
                                    ]
                                    $ subtypeTest False SRSingle "A Number" "B Number"
                                ]
                            , tGroup
                                "sub"
                                [ tDecls
                                    [ "subtype trustme A Integer <: B Number = fn Mk.A x => Mk.B x"
                                    , "subtype trustme A Number <: B Integer = fn Mk.A _ => Mk.B 0"
                                    ]
                                    $ subtypeTest False SRSingle "A Number" "B Integer"
                                , tDecls
                                    [ "subtype trustme A Number <: B Integer = fn Mk.A _ => Mk.B 0"
                                    , "subtype trustme A Integer <: B Number = fn Mk.A x => Mk.B x"
                                    ]
                                    $ subtypeTest False SRSingle "A Number" "B Integer"
                                ]
                            , tGroup
                                "in"
                                [ tDecls
                                    [ "subtype trustme A Integer <: C = fn Mk.A _ => Mk.C"
                                    , "subtype trustme A Number <: C = fn Mk.A _ => Mk.C"
                                    ]
                                    $ subtypeTest False SRSingle "A Number" "C"
                                , tDecls
                                    [ "subtype trustme A Number <: C = fn Mk.A _ => Mk.C"
                                    , "subtype trustme A Integer <: C = fn Mk.A _ => Mk.C"
                                    ]
                                    $ subtypeTest False SRSingle "A Number" "C"
                                ]
                            , tGroup
                                "out"
                                [ tDecls
                                    [ "subtype trustme C <: B Integer = fn Mk.C => Mk.B 0"
                                    , "subtype trustme C <: B Number = fn Mk.C => Mk.B 0"
                                    ]
                                    $ subtypeTest False SRSingle "C" "B Integer"
                                , tDecls
                                    [ "subtype trustme C <: B Number = fn Mk.C => Mk.B 0"
                                    , "subtype trustme C <: B Integer = fn Mk.C => Mk.B 0"
                                    ]
                                    $ subtypeTest False SRSingle "C" "B Integer"
                                ]
                            ]
                        , tDecls ["subtype List (A a) <: A (List a) = fn aa => Mk.A $ map.List (fn Mk.A a => a) aa"]
                            $ tGroup
                                "monoid"
                                [ testExpectSuccess "pass"
                                , subtypeTest False SRSingle "List (A Integer)" "A (List Integer)"
                                , subtypeTest True SRSubsume "List (A None)" "A (List None)"
                                ]
                        ]
                ]
            , testOpenUHStore
                $ tWith ["Store", "UndoHandler"]
                $ tDecls
                    [ "entitytype E"
                    , "eta = !{property @E @Text !\"eta\"} store"
                    , "e1 = !{point.OpenEntity @E !\"e1\"}"
                    , "rt1 = eta !$ ap{e1}"
                    ]
                $ tGroup
                    "undo"
                    [ testExpectSuccess "do {rt1 := \"A\"; testrefeq ap{\"A\"} rt1; rt1 := \"B\"; testrefeq ap{\"B\"} rt1;}"
                    , testExpectSuccess
                        "do {rt1 := \"A\"; testrefeq ap{\"A\"} rt1; rt1 := \"B\"; testrefeq ap{\"B\"} rt1; queueUndo undoHandler; testrefeq ap{\"A\"} rt1;}"
                    , testExpectSuccess
                        "do {rt1 := \"A\"; testrefeq ap{\"A\"} rt1; rt1 := \"B\"; testrefeq ap{\"B\"} rt1; queueUndo undoHandler; testrefeq ap{\"A\"} rt1; queueRedo undoHandler; testrefeq ap{\"B\"} rt1;}"
                    ]
            , tGroup
                "interpret"
                [ testExpectSuccess "do {r <- newMem.WholeModel; asText.Integer !$ r := \"37\"; testrefeq ap{37} r;}"
                , testExpectSuccess
                    "do {r <- newMem.WholeModel; asText.Date !$ r := \"2015-08-12\"; testrefeq ap{YearMonthDay 2015 08 12} r;}"
                ]
            , tDecls
                [ "runresult = fn ar, arg => ar >- fn {Failure err => fail $ show err; Success f => f arg}"
                , "testaction = fn expected, action => do {found <- action; testeq expected found}"
                , "testFailure = fn action => do {found <- action; found >- fn {Failure _ => pass; Success _ => fail \"not Failure\"}}"
                ]
                $ tWith ["Pinafore"]
                $ tDecls
                    [ "evaluate = fn t, text => map.Action (mapFailure.Result show) $ run.Interpreter !{this.Context} $ do.Interpreter { v <- interpret.Value text; unify.Value !{this.Scope} t v }"
                    ]
                $ tGroup
                    "evaluate"
                    [ testExpectSuccess "pass"
                    , testExpectSuccess "testaction (Success True) $ evaluate @Boolean \"True\""
                    , testExpectSuccess "testaction (Success 5) $ evaluate @Integer \"5\""
                    , testExpectSuccess "testaction (Success 5) $ evaluate @Integer \"let {x = 5} x\""
                    , testExpectSuccess
                        "do {ar <- evaluate @(Integer -> Integer) \"fn x => x +.Integer 1\"; ar >- fn {Failure err => fail err; Success f => testeq 8 $ f 7}}"
                    , testExpectSuccess
                        "testaction (Failure \"<evaluate>:1:1: syntax: expecting: expression\") $ evaluate @Integer \"\""
                    , testExpectSuccess "testaction (Failure \"<evaluate>:1:1: undefined: f: a\") $ evaluate @Integer \"f\""
                    , testExpectSuccess "testFailure $ evaluate @Integer \"\\\"hello\\\"\""
                    , testExpectSuccess
                        "do {r <- newMem.WholeModel; ar <- evaluate @(WholeModel Integer -> Action Unit) \"fn r => r :=.WholeModel 45\"; runresult ar r; a <- get r; testeq 45 a;}"
                    , testExpectSuccess "testaction 569 $ evaluate @(a -> a) \"fn x => x\" >>= fn Success f => pure $ f 569"
                    , testExpectSuccess
                        "testaction 570 $ evaluate @(Integer -> Integer) \"fn x => x\" >>= fn Success f => pure $ f 570"
                    ]
            , tGroup
                "text-sort"
                [ testExpectSuccess "testeq EQ $ compare.Text \"a\" \"a\""
                , testExpectSuccess "testeq EQ $ compare.Text \"A\" \"A\""
                , testExpectSuccess "testeq LT $ compare.Text \"a\" \"A\""
                , testExpectSuccess "testeq LT $ compare.Text \"a\" \"b\""
                , testExpectSuccess "testeq LT $ compare.Text \"A\" \"b\""
                , testExpectSuccess "testeq LT $ compare.Text \"a\" \"B\""
                ]
            , tGroup
                "applicative-notation"
                [ testExpectSuccess "testeq (Just 3) $ ap.Maybe {3}"
                , testExpectSuccess "testeq [10,13,11,14] $ ap.List {%([3,4]) +.Integer %([7,10])}"
                ]
            , tGroup
                "do-notation"
                [ testExpectSuccess "testeq (Just 3) $ do.Maybe {pure 3}"
                , testExpectSuccess "testeq [10,13,11,14] $ do.List {a <- [3,4]; b <- [7,10]; pure $ a +.Integer b}"
                ]
            , tGroup
                "task"
                [ testExpectSuccess
                    "do {t <- async.Task $ do {sleep $ Seconds 0.01; pure True}; v <- wait.Task t; if v then pass else fail \"\"}"
                , testExpectSuccess
                    "do {r <- newMem.WholeModel; r := 0; t <- async.Task $ do {sleep $ Seconds 0.01; r := 1;}; wait.Task t; v <- get r; if v == 1 then pass else fail \"\"}"
                , testExpectSuccess
                    "do {r <- newMem.WholeModel; r := 0; t <- async.Task $ do {sleep $ Seconds 0.05; r := 1;}; v <- get r; if v == 0 then pass else fail \"\"}"
                , testExpectSuccess
                    "do {r <- newMem.WholeModel; r := 0; t <- run.Lifecycle $ async.Task $ do {sleep $ Seconds 0.05; r := 1;}; v <- get r; if v == 1 then pass else fail \"\"}"
                ]
            ]
