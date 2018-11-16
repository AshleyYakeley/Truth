module Test.Entity
    ( testEntity
    ) where

import Pinafore
import Shapes
import Test.Tasty
import Test.Tasty.HUnit
import Truth.Core
import Truth.World.ObjectStore

makeTestObject :: IO (Object PinaforeEdit)
makeTestObject = do
    tableStateObject :: Object (WholeEdit (EditSubject PinaforeTableEdit)) <- freeIOObject ([], []) $ \_ -> True
    return $
        tupleObject $ \case
            PinaforeSelectPoint -> pinaforeTablePointObject $ convertObject tableStateObject
            PinaforeSelectFile -> readConstantObject $ constFunctionReadFunction nullSingleObjectMutableRead

makeTestPinaforeContest :: IO PinaforeContext
makeTestPinaforeContest = do
    pinaforeObject <- makeTestObject
    makePinaforeContext pinaforeObject $ \_ -> return ()

defs :: [String]
defs =
    [
      "testeq expected found = if expected == found then pass else fail $ \"expected \" ++ totext expected ++ \" but found \" ++ totext found"
    , "testneq expected found = if not (expected == found) then pass else fail $ \"expected not \" ++ totext expected ++ \" but found \" ++ totext found"
    -- , "testisnull t = if exists t then fail (\"expected null but found \" ++ t) else pass"
    , "entity T"
    , "ma = property @T @T %3f1b6c1e-06ea-454c-b446-58ccd23ffda1"
    , "mb = property @T @T %410a71b0-fc3c-415d-85eb-de8c1cb88267"
    , "mc = property @T @T %69aa80b4-b3fd-42aa-961a-d1a5ee0cf950"
    , "md = property @T @T %917589a7-e181-4b5e-918e-15dfd1729f0f"
    , "p1 = point @T %d4d3096a-b1f7-4cb1-8dfa-c907e57baed1"
    , "p2 = point @T %6a76c6b3-c9d6-4e76-af3a-863b7c46b34c"
    , "p3 = point @T %5048ecd6-bebb-4500-a508-f188b4cc7443"
    , "p4 = point @T %6ffe864b-d2c3-4857-8057-ef472475eb2b"
    ]

prefix :: Text
prefix = pack $ "let\n" ++ intercalate ";\n" defs ++ "\nin "

pointTest :: Text -> TestTree
pointTest text =
    testCase (unpack text) $ do
        pc <- makeTestPinaforeContest
        pinaforeRunFile pc "<test>" $ prefix <> text

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
              [ pointTest "testeq 1 1"
              , pointTest "testeq 1 \"1\""
              , pointTest "testeq false $ 0 == 1"
              , pointTest "testeq true $ 1 == 1"
              , pointTest "testeq false $ 1 == ~1"
              , pointTest "testeq true $ p1 == p1"
              , pointTest "testeq true $ (ma !$ p1) == (ma !$ p1)"
              ]
        , testGroup
              "null & exists"
              [ pointTest "if exists null then fail \"failed\" else pass"
              , pointTest "if exists p1 then fail \"failed\" else pass"
              , pointTest "pass >> if exists p1 then fail \"failed\" else pass"
              , pointTest "if exists p1 then fail \"failed\" else pass >> pass"
              , pointTest "testisnull null"
              , pointTest "testisnull (ma p1)"
              , pointTest "testisnull $ null ?? null"
              , pointTest "testeq 0 $ null ?? 0"
              , pointTest "testeq 1 $ 1 ?? 0"
              , pointTest "testeq 1 $ 1 ?? null"
              ]
        , testGroup
              ":="
              [ pointTest "ma !$ p1 := \"hello\""
              , pointTest "ma !$ p1 := p2"
              , pointTest "ma !$ p1 := p2 >> testeq p2 (ma p1)"
              , pointTest "ma !$ p1 := \"hello\" >> testeq \"hello\" (ma !$ p1)"
              ]
        , testGroup
              "+="
              [ pointTest "ma !@ \"hello\" += p1"
              , pointTest "ma !@ \"hello\" += p1 >> pass"
              , pointTest "ma !@ \"hello\" += p1 >> testeq \"hello\" (ma !$ p1)"
              ]
        , testGroup "-=" [pointTest "ma !@ \"hello\" += p1 >> ma !@ \"hello\" -= p1 >> testisnull (ma !$ p1)"]
        , testGroup "removeall" [pointTest "@ma !@ \"hello\" += p1 >> removeall (ma !$ \"hello\") >> testisnull (ma !$ p1)"]
        , testGroup "matching literals" [pointTest "ma !$ p1 := \"hello\" >> ma !$ p2 := \"hello\" >> testeq (ma !$ p1) (ma !$ p2)"]
        , testGroup
              "identity morphism"
              [ pointTest "(identity $ ma p1) := p2 >> testeq p2 (ma p1)"
              , pointTest "(ma $ identity p1) := p2 >> testeq p2 (ma p1)"
              , pointTest "((identity . ma) p1) := p2 >> testeq p2 (ma p1)"
              , pointTest "((ma . identity) p1) := p2 >> testeq p2 (ma p1)"
              , pointTest "ma p1 := p2 >> testeq p2 (identity $ ma p1)"
              , pointTest "ma p1 := p2 >> testeq p2 (ma $ identity p1)"
              , pointTest "ma p1 := p2 >> testeq p2 ((identity . ma) p1)"
              , pointTest "ma p1 := p2 >> testeq p2 ((ma . identity) p1)"
              , pointTest "(identity $ ma p1) := p2 >> testeq p2 (identity $ ma p1)"
              ]
        , testGroup
              "identity inverse morphism"
              [ pointTest "(@identity $ @ma \"hello\") += p1 >> testisnull (ma p1)"
              , pointTest "(immutableset $ @ma \"hello\") += p1 >> testisnull (ma p1)"
              , pointTest "(@ma $ @identity \"hello\") += p1 >> testisnull (ma p1)"
              , pointTest "(@ma $ immutableset [\"hello\"]) += p1 >> testisnull (ma p1)"
              , pointTest "((@identity . @ma) \"hello\") += p1 >> testeq \"hello\" (ma p1)"
              , pointTest "((@ma . @identity) \"hello\") += p1 >> testeq \"hello\" (ma p1)"
              , pointTest "@ma \"hello\" += p1 >> @ma \"hello\" -= p1 >> testisnull (ma p1)"
              , pointTest "@ma \"hello\" += p1 >> (@identity $ @ma \"hello\") -= p1 >> testeq \"hello\" (ma p1)"
              , pointTest "@ma \"hello\" += p1 >> (immutableset $ @ma \"hello\") -= p1 >> testeq \"hello\" (ma p1)"
              , pointTest "@ma \"hello\" += p1 >> (@ma $ @identity \"hello\") -= p1 >> testisnull (ma p1)"
              , pointTest "@ma \"hello\" += p1 >> (@ma $ immutableset [\"hello\"]) -= p1 >> testisnull (ma p1)"
              , pointTest "@ma \"hello\" += p1 >> ((@identity . @ma) \"hello\") -= p1 >> testisnull (ma p1)"
              , pointTest "@ma \"hello\" += p1 >> ((@ma . @identity) \"hello\") -= p1 >> testisnull (ma p1)"
              ]
        , testGroup
              "composed morphisms"
              [ pointTest "(ma $ mb p1) := p2 >> testeq p2 (ma $ mb p1)"
              , pointTest "(ma $ mb p1) := \"hello\" >> testeq \"hello\" (ma $ mb p1)"
              , pointTest "(ma . mb $ p1) := p2 >> testeq p2 (ma $ mb p1)"
              , pointTest "(ma . mb $ p1) := \"hello\" >> testeq \"hello\" (ma $ mb p1)"
              , pointTest "(ma $ mb p1) := p2 >> testeq p2 (ma . mb $ p1)"
              , pointTest "(ma $ mb p1) := \"hello\" >> testeq \"hello\" (ma . mb $ p1)"
              ]
        , testGroup
              "composed inverse morphisms"
              [ pointTest "(@mb $ @ma \"hello\") += p1 >> testeq \"hello\" (ma $ mb p1)"
              , pointTest "((@mb . @ma) \"hello\") += p1 >> testeq \"hello\" (ma $ mb p1)"
              , pointTest "((@mb . @ma) \"hello\") += p1 >> testisnull (mb $ ma p1)"
              , pointTest "mb p1 := p2 >> ((@mb . @ma) \"hello\") += p1 >> testeq p2 (mb p1)"
              , pointTest "mb p1 := p2 >> ((@mb . @ma) \"hello\") += p1 >> testeq \"hello\" (ma p2)"
              , pointTest "mb p1 := p2 >> ( @mb $ @ma  \"hello\") += p1 >> testneq p2 (mb p1)"
              , pointTest "mb p1 := p2 >> ( @mb $ @ma  \"hello\") += p1 >> testneq \"hello\" (ma p2)"
              , pointTest "mb p1 := p2 >> ma p2 := \"hello\" >> ((@mb . @ma) \"hello\") -= p1 >> testeq p2 (mb p1)"
              , pointTest "mb p1 := p2 >> ma p2 := \"hello\" >> ((@mb . @ma) \"hello\") -= p1 >> testisnull (ma p2)"
              , pointTest "mb p1 := p2 >> ma p2 := \"hello\" >> ( @mb $ @ma  \"hello\") -= p1 >> testneq p2 (mb p1)"
              , pointTest
                    "mb p1 := p2 >> ma p2 := \"hello\" >> ( @mb $ @ma  \"hello\") -= p1 >> testeq \"hello\" (ma p2)"
              , pointTest "mb p1 := p2 >> ma p2 := \"hello\" >> removeall ((@mb . @ma) \"hello\") >> testeq p2 (mb p1)"
              , pointTest "mb p1 := p2 >> ma p2 := \"hello\" >> removeall ((@mb . @ma) \"hello\") >> testisnull (ma p2)"
              , pointTest "mb p1 := p2 >> ma p2 := \"hello\" >> removeall ( @mb $ @ma  \"hello\") >> testneq p2 (mb p1)"
              , pointTest
                    "mb p1 := p2 >> ma p2 := \"hello\" >> removeall ( @mb $ @ma  \"hello\") >> testeq \"hello\" (ma p2)"
              ]
        , testGroup
              "single"
              [ pointTest "testisnull (single $ mb $ @ma 0)"
              , pointTest "mb p1 := 1 >> ma p1 := 0 >> testeq 1 (single $ mb $ @ma 0)"
              , pointTest "mb p1 := 1 >> ma p1 := 0 >> mc p1 := 0 >> testeq 1 (single $ mb $ @ma 0)"
              , pointTest "mb p1 := 1 >> ma p1 := 0 >> ma p1 := 0 >> testeq 1 (single $ mb $ @ma 0)"
              , pointTest "mb p1 := 1 >> mb p2 := 2 >> ma p1 := 0 >> ma p2 := 0 >> testisnull (single $ mb $ @ma 0)"
              , pointTest "mb p1 := 1 >> mb p2 := 1 >> ma p1 := 0 >> ma p2 := 0 >> testeq 1 (single $ mb $ @ma 0)"
              ]
        , testGroup
              "null set member"
              [ pointTest "@ma p1 += null >> testeq 0 (count (@ma p1))"
              , pointTest "@ma p1 += null >> @ma p1 += 10 >> testeq 1 (count (@ma p1))"
              , pointTest "@ma p1 += null >> @ma p1 += 10 >> testeq 10 (sum (@ma p1))"
              , pointTest "@ma p1 += null >> @ma p1 += 10 >> testeq 10 (mean (@ma p1))"
              , pointTest "@ma p1 += null >> @ma p1 += 10 >> testeq 10 (single $ @ma p1)"
              , pointTest $
                "let counter = ma p1 in " <>
                "counter := 0 >> @ma p1 += null >> @ma p1 += 10 >> withset (orders []) (@ma p1) (\\p -> counter := counter + 1) >> testeq 2 counter"
              , pointTest $
                "let counter = ma p1 in " <>
                "counter := 0 >> @ma p1 += null >> @ma p1 += null >> @ma p1 += 10 >> withset (orders []) (@ma p1) (\\p -> counter := counter + 1) >> testeq 3 counter"
              ]
        , testGroup
              "multiple set member"
              [ pointTest "@ma p1 += 1 >> @ma p1 += 1 >> testeq 1 (count (@ma p1))"
              , pointTest $
                "let counter = ma p1 in " <>
                "counter := 0 >> @ma p1 += 1 >> @ma p1 += 1 >> withset (orders []) (@ma p1) (\\p -> counter := counter + 1) >> testeq 1 counter"
              ]
        ]
