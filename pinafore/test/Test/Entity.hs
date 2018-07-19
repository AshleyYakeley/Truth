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
    [ "ornull a b = if exists a then a else b"
    , "testeq expected found = if is expected found then pass else fail (\"expected \" ++ (ornull expected \"null\") ++ \" but found \" ++ (ornull found \"null\"))"
    , "testneq expected found = if not (is expected found) then pass else fail (\"expected not \" ++ (ornull expected \"null\") ++ \" but found \" ++ (ornull found \"null\"))"
    , "testisnull t = if exists t then fail (\"expected null but found \" ++ t) else pass"
    , "ma = %3f1b6c1e-06ea-454c-b446-58ccd23ffda1"
    , "mb = %410a71b0-fc3c-415d-85eb-de8c1cb88267"
    , "mc = %69aa80b4-b3fd-42aa-961a-d1a5ee0cf950"
    , "md = %917589a7-e181-4b5e-918e-15dfd1729f0f"
    , "p1 = !d4d3096a-b1f7-4cb1-8dfa-c907e57baed1"
    , "p2 = !6a76c6b3-c9d6-4e76-af3a-863b7c46b34c"
    , "p3 = !5048ecd6-bebb-4500-a508-f188b4cc7443"
    , "p4 = !6ffe864b-d2c3-4857-8057-ef472475eb2b"
    ]

prefix :: Text
prefix = pack $ "let " ++ intercalate ";" defs ++ " in "

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
              , pointTest "testeq false $ is 0 1"
              , pointTest "testeq true $ is 1 1"
              , pointTest "testeq false $ is 1 ~1"
              , pointTest "testeq false $ is null 1"
              , pointTest "testeq false $ is null null"
              , pointTest "let p = null in testeq false $ is p p"
              , pointTest "testeq true $ is p1 p1"
              , pointTest "testeq true $ is (ma p1) (ma p1)"
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
              "setentity"
              [ pointTest "setentity (ma p1) \"hello\""
              , pointTest "setentity (ma p1) p2"
              , pointTest "setentity (ma p1) p2 >> testeq p2 (ma p1)"
              , pointTest "setentity (ma p1) \"hello\" >> testeq \"hello\" (ma p1)"
              ]
        , testGroup
              "addentity"
              [ pointTest "addentity (@ma \"hello\") p1"
              , pointTest "addentity (@ma \"hello\") p1 >> pass"
              , pointTest "addentity (@ma \"hello\") p1 >> testeq \"hello\" (ma p1)"
              ]
        , testGroup
              "removeentity"
              [pointTest "addentity (@ma \"hello\") p1 >> removeentity (@ma \"hello\") p1 >> testisnull (ma p1)"]
        , testGroup
              "removeall"
              [pointTest "addentity (@ma \"hello\") p1 >> removeall (@ma \"hello\") >> testisnull (ma p1)"]
        , testGroup
              "matching literals"
              [pointTest "setentity (ma p1) \"hello\" >> setentity (ma p2) \"hello\" >> testeq (ma p1) (ma p2)"]
        , testGroup
              "identity morphism"
              [ pointTest "setentity (identity $ ma p1) p2 >> testeq p2 (ma p1)"
              , pointTest "setentity (ma $ identity p1) p2 >> testeq p2 (ma p1)"
              , pointTest "setentity ((identity . ma) p1) p2 >> testeq p2 (ma p1)"
              , pointTest "setentity ((ma . identity) p1) p2 >> testeq p2 (ma p1)"
              , pointTest "setentity (ma p1) p2 >> testeq p2 (identity $ ma p1)"
              , pointTest "setentity (ma p1) p2 >> testeq p2 (ma $ identity p1)"
              , pointTest "setentity (ma p1) p2 >> testeq p2 ((identity . ma) p1)"
              , pointTest "setentity (ma p1) p2 >> testeq p2 ((ma . identity) p1)"
              , pointTest "setentity (identity $ ma p1) p2 >> testeq p2 (identity $ ma p1)"
              ]
        , testGroup
              "identity inverse morphism"
              [ pointTest "addentity (@identity $ @ma \"hello\") p1 >> testisnull (ma p1)"
              , pointTest "addentity (immutableset $ @ma \"hello\") p1 >> testisnull (ma p1)"
              , pointTest "addentity (@ma $ @identity \"hello\") p1 >> testisnull (ma p1)"
              , pointTest "addentity (@ma $ immutableset [\"hello\"]) p1 >> testisnull (ma p1)"
              , pointTest "addentity ((@identity . @ma) \"hello\") p1 >> testeq \"hello\" (ma p1)"
              , pointTest "addentity ((@ma . @identity) \"hello\") p1 >> testeq \"hello\" (ma p1)"
              , pointTest "addentity (@ma \"hello\") p1 >> removeentity (@ma \"hello\") p1 >> testisnull (ma p1)"
              , pointTest
                    "addentity (@ma \"hello\") p1 >> removeentity (@identity $ @ma \"hello\") p1 >> testeq \"hello\" (ma p1)"
              , pointTest
                    "addentity (@ma \"hello\") p1 >> removeentity (immutableset $ @ma \"hello\") p1 >> testeq \"hello\" (ma p1)"
              , pointTest
                    "addentity (@ma \"hello\") p1 >> removeentity (@ma $ @identity \"hello\") p1 >> testisnull (ma p1)"
              , pointTest
                    "addentity (@ma \"hello\") p1 >> removeentity (@ma $ immutableset [\"hello\"]) p1 >> testisnull (ma p1)"
              , pointTest
                    "addentity (@ma \"hello\") p1 >> removeentity ((@identity . @ma) \"hello\") p1 >> testisnull (ma p1)"
              , pointTest
                    "addentity (@ma \"hello\") p1 >> removeentity ((@ma . @identity) \"hello\") p1 >> testisnull (ma p1)"
              ]
        , testGroup
              "composed morphisms"
              [ pointTest "setentity (ma $ mb p1) p2 >> testeq p2 (ma $ mb p1)"
              , pointTest "setentity (ma $ mb p1) \"hello\" >> testeq \"hello\" (ma $ mb p1)"
              , pointTest "setentity (ma . mb $ p1) p2 >> testeq p2 (ma $ mb p1)"
              , pointTest "setentity (ma . mb $ p1) \"hello\" >> testeq \"hello\" (ma $ mb p1)"
              , pointTest "setentity (ma $ mb p1) p2 >> testeq p2 (ma . mb $ p1)"
              , pointTest "setentity (ma $ mb p1) \"hello\" >> testeq \"hello\" (ma . mb $ p1)"
              ]
        , testGroup
              "composed inverse morphisms"
              [ pointTest "addentity (@mb $ @ma \"hello\") p1 >> testeq \"hello\" (ma $ mb p1)"
              , pointTest "addentity ((@mb . @ma) \"hello\") p1 >> testeq \"hello\" (ma $ mb p1)"
              , pointTest "addentity ((@mb . @ma) \"hello\") p1 >> testisnull (mb $ ma p1)"
              , pointTest "setentity (mb p1) p2 >> addentity ((@mb . @ma) \"hello\") p1 >> testeq p2 (mb p1)"
              , pointTest "setentity (mb p1) p2 >> addentity ((@mb . @ma) \"hello\") p1 >> testeq \"hello\" (ma p2)"
              , pointTest "setentity (mb p1) p2 >> addentity ( @mb $ @ma  \"hello\") p1 >> testneq p2 (mb p1)"
              , pointTest "setentity (mb p1) p2 >> addentity ( @mb $ @ma  \"hello\") p1 >> testneq \"hello\" (ma p2)"
              , pointTest
                    "setentity (mb p1) p2 >> setentity (ma p2) \"hello\" >> removeentity ((@mb . @ma) \"hello\") p1 >> testeq p2 (mb p1)"
              , pointTest
                    "setentity (mb p1) p2 >> setentity (ma p2) \"hello\" >> removeentity ((@mb . @ma) \"hello\") p1 >> testisnull (ma p2)"
              , pointTest
                    "setentity (mb p1) p2 >> setentity (ma p2) \"hello\" >> removeentity ( @mb $ @ma  \"hello\") p1 >> testneq p2 (mb p1)"
              , pointTest
                    "setentity (mb p1) p2 >> setentity (ma p2) \"hello\" >> removeentity ( @mb $ @ma  \"hello\") p1 >> testeq \"hello\" (ma p2)"
              , pointTest
                    "setentity (mb p1) p2 >> setentity (ma p2) \"hello\" >> removeall ((@mb . @ma) \"hello\") >> testeq p2 (mb p1)"
              , pointTest
                    "setentity (mb p1) p2 >> setentity (ma p2) \"hello\" >> removeall ((@mb . @ma) \"hello\") >> testisnull (ma p2)"
              , pointTest
                    "setentity (mb p1) p2 >> setentity (ma p2) \"hello\" >> removeall ( @mb $ @ma  \"hello\") >> testneq p2 (mb p1)"
              , pointTest
                    "setentity (mb p1) p2 >> setentity (ma p2) \"hello\" >> removeall ( @mb $ @ma  \"hello\") >> testeq \"hello\" (ma p2)"
              ]
        , testGroup
              "single"
              [ pointTest "testisnull (single $ mb $ @ma 0)"
              , pointTest "setentity (mb p1) 1 >> setentity (ma p1) 0 >> testeq 1 (single $ mb $ @ma 0)"
              , pointTest
                    "setentity (mb p1) 1 >> setentity (ma p1) 0 >> setentity (mc p1) 0 >> testeq 1 (single $ mb $ @ma 0)"
              , pointTest
                    "setentity (mb p1) 1 >> setentity (ma p1) 0 >> setentity (ma p1) 0 >> testeq 1 (single $ mb $ @ma 0)"
              , pointTest
                    "setentity (mb p1) 1 >> setentity (mb p2) 2 >> setentity (ma p1) 0 >> setentity (ma p2) 0 >> testisnull (single $ mb $ @ma 0)"
              , pointTest
                    "setentity (mb p1) 1 >> setentity (mb p2) 1 >> setentity (ma p1) 0 >> setentity (ma p2) 0 >> testeq 1 (single $ mb $ @ma 0)"
              ]
        , testGroup
              "null set member"
              [ pointTest "addentity (@ma p1) null >> testeq 0 (count (@ma p1))"
              , pointTest "addentity (@ma p1) null >> addentity (@ma p1) 10 >> testeq 1 (count (@ma p1))"
              , pointTest "addentity (@ma p1) null >> addentity (@ma p1) 10 >> testeq 10 (sum (@ma p1))"
              , pointTest "addentity (@ma p1) null >> addentity (@ma p1) 10 >> testeq 10 (mean (@ma p1))"
              , pointTest "addentity (@ma p1) null >> addentity (@ma p1) 10 >> testeq 10 (single $ @ma p1)"
              , pointTest $
                "let counter = ma p1 in " <>
                "setentity counter 0 >> addentity (@ma p1) null >> addentity (@ma p1) 10 >> withset (orders []) (@ma p1) (\\p -> setentity counter (counter + 1)) >> testeq 2 counter"
              , pointTest $
                "let counter = ma p1 in " <>
                "setentity counter 0 >> addentity (@ma p1) null >> addentity (@ma p1) null >> addentity (@ma p1) 10 >> withset (orders []) (@ma p1) (\\p -> setentity counter (counter + 1)) >> testeq 3 counter"
              ]
        , testGroup
              "multiple set member"
              [ pointTest "addentity (@ma p1) 1 >> addentity (@ma p1) 1 >> testeq 1 (count (@ma p1))"
              , pointTest $
                "let counter = ma p1 in " <>
                "setentity counter 0 >> addentity (@ma p1) 1 >> addentity (@ma p1) 1 >> withset (orders []) (@ma p1) (\\p -> setentity counter (counter + 1)) >> testeq 1 counter"
              ]
        ]
