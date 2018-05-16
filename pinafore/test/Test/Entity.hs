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
    , "testisnull t = if exists t then fail (\"expected null but found \" ++ t) else pass"
    , "ma = %3f1b6c1e-06ea-454c-b446-58ccd23ffda1"
    , "mb = %410a71b0-fc3c-415d-85eb-de8c1cb88267"
    , "mc = %69aa80b4-b3fd-42aa-961a-d1a5ee0cf950"
    , "md = %917589a7-e181-4b5e-918e-15dfd1729f0f"
    , "p1 = !d4d3096a-b1f7-4cb1-8dfa-c907e57baed1"
    , "p2 = !6a76c6b3-c9d6-4e76-af3a-863b7c46b34c"
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
        [ pointTest "pass"
        , pointTest "pass >> pass"
        , pointTest "if true then pass else fail \"failed\""
        , pointTest "pass >> if true then pass else fail \"failed\""
        -- equality
        , pointTest "testeq 1 1"
        , pointTest "testeq 1 \"1\""
        , pointTest "testeq false $ is 0 1"
        , pointTest "testeq true $ is 1 1"
        , pointTest "testeq false $ is 1 ~1"
        , pointTest "testeq false $ is null 1"
        , pointTest "testeq true $ is null null"
        -- null & exists
        , pointTest "if exists null then fail \"failed\" else pass"
        , pointTest "if exists p1 then fail \"failed\" else pass"
        , pointTest "pass >> if exists p1 then fail \"failed\" else pass"
        , pointTest "if exists p1 then fail \"failed\" else pass >> pass"
        , pointTest "testisnull null"
        , pointTest "testisnull (ma p1)"
        , pointTest "testisnull $ null ?? null"
        , pointTest "testeq 0 $ null ?? 0"
        , pointTest "testeq 1 $ 1 ?? 0"
        , pointTest "testeq 1 $ 1 ?? null"
        -- setentity
        , pointTest "setentity (ma p1) \"hello\""
        , pointTest "setentity (ma p1) p2"
        , pointTest "setentity (ma p1) p2 >> testeq p2 (ma p1)"
        , pointTest "setentity (ma p1) \"hello\" >> testeq \"hello\" (ma p1)"
        -- addentity
        , pointTest "addentity (@ma \"hello\") p1"
        , pointTest "addentity (@ma \"hello\") p1 >> pass"
        , pointTest "addentity (@ma \"hello\") p1 >> testeq \"hello\" (ma p1)"
        -- matching literals
        , pointTest "setentity (ma p1) \"hello\" >> setentity (ma p2) \"hello\" >> testeq (ma p1) (ma p2)"
        -- composed morphisms
        , pointTest "setentity (ma $ mb p1) p2 >> testeq p2 (ma $ mb p1)"
        , pointTest "setentity (ma $ mb p1) \"hello\" >> testeq \"hello\" (ma $ mb p1)"
        , pointTest "setentity (ma . mb $ p1) p2 >> testeq p2 (ma $ mb p1)"
        , pointTest "setentity (ma . mb $ p1) \"hello\" >> testeq \"hello\" (ma $ mb p1)"
        , pointTest "setentity (ma $ mb p1) p2 >> testeq p2 (ma . mb $ p1)"
        , pointTest "setentity (ma $ mb p1) \"hello\" >> testeq \"hello\" (ma . mb $ p1)"
        -- single
        , pointTest "testisnull (single $ mb $ @ma 0)"
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
