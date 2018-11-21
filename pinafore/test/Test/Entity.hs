module Test.Entity
    ( testEntity
    ) where

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
    , "testisunknown t = runref {if %(known t) then fail \"known\" else pass}"
    , "testeqval e f = testeq {e} {f}"
    , "entity T"
    , "ma = property @T @T !\"ma\""
    , "mb = property @T @T !\"mb\""
    , "mc = property @T @T !\"mc\""
    , "md = property @T @T !\"md\""
    , "la = property @T @Text !\"la\""
    , "na = property @T @Number !\"na\""
    , "nb = property @T @Number !\"nb\""
    , "nc = property @T @Number !\"nb\""
    , "ra = property @Text @T !\"ra\""
    , "rna = property @Number @T !\"rna\""
    , "p1 = point @T !d4d3096a-b1f7-4cb1-8dfa-c907e57baed1"
    , "p2 = point @T !6a76c6b3-c9d6-4e76-af3a-863b7c46b34c"
    , "p3 = point @T !5048ecd6-bebb-4500-a508-f188b4cc7443"
    , "p4 = point @T !6ffe864b-d2c3-4857-8057-ef472475eb2b"
    ]

prefix :: Text
prefix = pack $ "let\n" ++ intercalate ";\n" defs ++ "\nin\n"

pointTest :: Text -> TestTree
pointTest text =
    testCase (unpack text) $ do
        pc <- makeTestPinaforeContext
        _action <- pinaforeInterpretFile pc "<test>" $ prefix <> text
        return ()

testEntity :: TestTree
testEntity =
    testGroup
        "entity"
        [ testGroup
              "pass"
              [ pointTest "pass"
              , pointTest "pass >> pass"
              , pointTest "if true then pass else fail \"failed\""
              , pointTest "pass >> if true then pass else fail \"failed\""
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
              [ pointTest "testisunknown {% (la !$ {p1}) == % (la !$ {p1})}"
              , pointTest "runreforfail {if %(known unknown) then fail \"failed\" else pass}"
              , pointTest "runreforfail {if %(known $ la !$ {p1}) then fail \"failed\" else pass}"
              , pointTest "pass >> runreforfail {if %(known $ la !$ {p1}) then fail \"failed\" else pass}"
              , pointTest "runreforfail {pass >> if %(known $ la !$ {p1}) then fail \"failed\" else pass}"
              , pointTest "runreforfail {if %(known $ la !$ {p1}) then fail \"failed\" else pass} >> pass"
              , pointTest "testisunknown unknown"
              , pointTest "testisunknown (la !$ {p1})"
              , pointTest "testisunknown $ unknown ?? unknown"
              , pointTest "testeq {0} $ unknown ?? {0}"
              , pointTest "testeq {1} $ {1} ?? {0}"
              , pointTest "testeq {1} $ {1} ?? unknown"
              ]
        , testGroup
              ":="
              [ pointTest "la !$ {p1} := \"hello\""
              , pointTest "ma !$ {p1} := p2"
              , pointTest "ma !$ {p1} := p2 >> testeq {p2} (ma !$ {p1})"
              , pointTest "la !$ {p1} := \"hello\" >> testeq {\"hello\"} (la !$ {p1})"
              ]
        , testGroup
              "+="
              [ pointTest "la !@ {\"hello\"} += p1"
              , pointTest "la !@ {\"hello\"} += p1 >> pass"
              , pointTest "la !@ {\"hello\"} += p1 >> testeq {\"hello\"} (la !$ {p1})"
              ]
        , testGroup "-=" [pointTest "la !@ {\"hello\"} += p1 >> la !@ {\"hello\"} -= p1 >> testisunknown (la !$ {p1})"]
        , testGroup
              "removeall"
              [pointTest "la !@ {\"hello\"} += p1 >> removeall (la !@ {\"hello\"}) >> testisunknown (la !$ {p1})"]
        , testGroup
              "matching literals"
              [pointTest "la !$ {p1} := \"hello\" >> la !$ {p2} := \"hello\" >> testeq (la !$ {p1}) (la !$ {p2})"]
        , testGroup
              "identity morphism"
              [ pointTest "(identity !$ ma !$ {p1}) := p2 >> testeq {p2} (ma !$ {p1})"
              , pointTest "(ma !$ identity !$ {p1}) := p2 >> testeq {p2} (ma !$ {p1})"
              , pointTest "((identity !. ma) !$ {p1}) := p2 >> testeq {p2} (ma !$ {p1})"
              , pointTest "((ma !. identity) !$ {p1}) := p2 >> testeq {p2} (ma !$ {p1})"
              , pointTest "ma !$ {p1} := p2 >> testeq {p2} (identity !$ ma !$ {p1})"
              , pointTest "ma !$ {p1} := p2 >> testeq {p2} (ma !$ identity !$ {p1})"
              , pointTest "ma !$ {p1} := p2 >> testeq {p2} ((identity !. ma) !$ {p1})"
              , pointTest "ma !$ {p1} := p2 >> testeq {p2} ((ma !. identity) !$ {p1})"
              , pointTest "(identity !$ ma !$ {p1}) := p2 >> testeq {p2} (identity !$ ma !$ {p1})"
              ]
{-
        , testGroup
              "identity inverse morphism"
              [ pointTest "(identity !@@ la !@ {\"hello\"}) += p1 >> testisunknown (la !$ {p1})"
              , pointTest "(immutableset $ la !@ {\"hello\"}) += p1 >> testisunknown (la !$ {p1})"
              , pointTest "(la !@@ identity !@ {\"hello\"}) += p1 >> testisunknown (la !$ {p1})"
              , pointTest "(la !@@ immutableset [{\"hello\"}]) += p1 >> testisunknown (la !$ {p1})"
              , pointTest "((identity !. la) !@ {\"hello\"}) += p1 >> testeq {\"hello\"} (la !$ {p1})"
              , pointTest "((la !. identity) !@ {\"hello\"}) += p1 >> testeq {\"hello\"} (la !$ {p1})"
              , pointTest "la !@ {\"hello\"} += p1 >> la !@ {\"hello\"} -= p1 >> testisunknown (ma !$ {p1})"
              , pointTest
                    "la !@ {\"hello\"} += p1 >> (identity !@@ la !@ {\"hello\"}) -= p1 >> testeq {\"hello\"} (ma !$ {p1})"
              , pointTest
                    "la !@ {\"hello\"} += p1 >> (immutableset $ la !@ {\"hello\"}) -= p1 >> testeq {\"hello\"} (ma !$ {p1})"
              , pointTest
                    "la !@ {\"hello\"} += p1 >> (la !@ identity !@ {\"hello\"}) -= p1 >> testisunknown (ma !$ {p1})"
              , pointTest "la !@ {\"hello\"} += p1 >> (la !@ pureset [{\"hello\"}]) -= p1 >> testisunknown (ma !$ {p1})"
              , pointTest
                    "la !@ {\"hello\"} += p1 >> ((identity !. ma) !@ {\"hello\"}) -= p1 >> testisunknown (ma !$ {p1})"
              , pointTest
                    "la !@ {\"hello\"} += p1 >> ((la !. identity) !@ {\"hello\"}) -= p1 >> testisunknown (ma !$ {p1})"
              ]
-}
        , testGroup
              "composed morphisms"
              [ pointTest "(ma !$ mb !$ {p1}) := p2 >> testeq {p2} (ma !$ mb !$ {p1})"
              , pointTest "(la !$ mb !$ {p1}) := \"hello\" >> testeq {\"hello\"} (la !$ mb !$ {p1})"
              , pointTest "(ma !. mb !$ {p1}) := p2 >> testeq {p2} (ma !$ mb !$ {p1})"
              , pointTest "(la !. mb !$ {p1}) := \"hello\" >> testeq {\"hello\"} (la !$ mb !$ {p1})"
              , pointTest "(ma !$ mb !$ {p1}) := p2 >> testeq {p2} (ma !. mb !$ {p1})"
              , pointTest "(la !$ mb !$ {p1}) := \"hello\" >> testeq {\"hello\"} (la !. mb !$ {p1})"
              ]
{-
        , testGroup
              "composed inverse morphisms"
              [ pointTest "(mb !@@ la !@ {\"hello\"}) += p1 >> testeq {\"hello\"} (la !$ mb !$ {p1})"
              , pointTest "((@mb . @ma) {\"hello\"}) += p1 >> testeq {\"hello\"} (la !$ mb !$ {p1})"
              , pointTest "((@mb . @ma) {\"hello\"}) += p1 >> testisunknown (mb $ ma p1)"
              , pointTest "mb p1 := p2 >> ((@mb . @ma) {\"hello\"}) += p1 >> testeq p2 (mb p1)"
              , pointTest "mb p1 := p2 >> ((@mb . @ma) {\"hello\"}) += p1 >> testeq {\"hello\"} (ma p2)"
              , pointTest "mb p1 := p2 >> ( @mb $ @ma  {\"hello\"}) += p1 >> testneq p2 (mb p1)"
              , pointTest "mb p1 := p2 >> ( @mb $ @ma  {\"hello\"}) += p1 >> testneq {\"hello\"} (ma p2)"
              , pointTest "mb p1 := p2 >> ma p2 := {\"hello\"} >> ((@mb . @ma) {\"hello\"}) -= p1 >> testeq p2 (mb p1)"
              , pointTest
                    "mb p1 := p2 >> ma p2 := {\"hello\"} >> ((@mb . @ma) {\"hello\"}) -= p1 >> testisunknown (ma p2)"
              , pointTest "mb p1 := p2 >> ma p2 := {\"hello\"} >> ( @mb $ @ma  {\"hello\"}) -= p1 >> testneq p2 (mb p1)"
              , pointTest
                    "mb p1 := p2 >> ma p2 := {\"hello\"} >> ( @mb $ @ma  {\"hello\"}) -= p1 >> testeq {\"hello\"} (ma p2)"
              , pointTest
                    "mb p1 := p2 >> ma p2 := {\"hello\"} >> removeall ((@mb . @ma) {\"hello\"}) >> testeq p2 (mb p1)"
              , pointTest
                    "mb p1 := p2 >> ma p2 := {\"hello\"} >> removeall ((@mb . @ma) {\"hello\"}) >> testisunknown (ma p2)"
              , pointTest
                    "mb p1 := p2 >> ma p2 := {\"hello\"} >> removeall ( @mb $ @ma  {\"hello\"}) >> testneq p2 (mb p1)"
              , pointTest
                    "mb p1 := p2 >> ma p2 := {\"hello\"} >> removeall ( @mb $ @ma  {\"hello\"}) >> testeq {\"hello\"} (ma p2)"
              ]
 -}
        , testGroup
              "single"
              [ pointTest "testisunknown (single $ nb !$$ na !@ {0})"
              , pointTest "nb !$ {p1} := 1 >> na !$ {p1} := 0 >> testeq {1} (single $ nb !$$ na !@ {0})"
              , pointTest
                    "nb !$ {p1} := 1 >> na !$ {p1} := 0 >> nc !$ {p1} := 0 >> testeq {1} (single $ nb !$$ na !@ {0})"
              , pointTest
                    "nb !$ {p1} := 1 >> na !$ {p1} := 0 >> na !$ {p1} := 0 >> testeq {1} (single $ nb !$$ na !@ {0})"
              , pointTest
                    "nb !$ {p1} := 1 >> nb !$ {p2} := 2 >> na !$ {p1} := 0 >> na !$ {p2} := 0 >> testisunknown (single $ nb !$$ na !@ {0})"
              , pointTest
                    "nb !$ {p1} := 1 >> nb !$ {p2} := 1 >> na !$ {p1} := 0 >> na !$ {p2} := 0 >> testeq {1} (single $ nb !$$ na !@ {0})"
              ]
        , testGroup
              "multiple set member"
              [ pointTest "ra !@ {p1} += \"hello\" >> ra !@ {p1} += \"hello\" >> testeq {1} (count (ra !@ {p1}))"
              , pointTest "ra !@ {p1} += \"h\" >> ra !@ {p1} += \"hello\" >> testeq {2} (count (ra !@ {p1}))"
              , pointTest $
                "let counter = na !$ {p1};someset = rna !@ {p1} in " <>
                "counter := 0 >> someset += 1 >> someset += 1 >> (with (members (orders []) someset) $ \\pp -> for pp $ \\p -> runref {counter := %counter + 1}) >> testeq {1} counter"
              ]
        ]
