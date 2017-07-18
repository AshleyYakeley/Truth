module Main(main) where
{
    import Control.Exception(evaluate);
    import Data.Type.Heterogeneous;
    import Language.Haskell.TH hiding (Info);
    import Language.Haskell.TH.SimpleType;
    import Data.Reity;
    import Test.Tasty;
    import Test.Tasty.HUnit;
    import SimpleType;


    testMatch :: (String,Maybe (String,String)) -> TestTree;
    testMatch (name,result) = testCase name $ case result of
    {
        Nothing -> return ();
        Just (expected,found) -> assertEqual "types" expected found;
    };

    type T2 a b = (a,b);
    type family TF2 a b;

    $(return []);
    testSimpleType :: TestTree;
    testSimpleType = testGroup "SimpleType"
    [
        testMatch $(do
        {
            boolType <- [t|Bool|];
            charType <- [t|Char|];
            testType <- [t|(Bool,Char)|];
            testToSimpleExpr testType $ AppType (AppType (ConsType (TupleT 2)) (ConsType boolType)) (ConsType charType);
        }),
        testMatch $(do
        {
            boolType <- [t|Bool|];
            charType <- [t|Char|];
            testType <- [t|T2 Bool Char|];
            testToSimpleExpr testType $ AppType (AppType (ConsType (TupleT 2)) (ConsType boolType)) (ConsType charType);
        }),
        testMatch $(do
        {
            boolType <- [t|Bool|];
            charType <- [t|Char|];
            testType <- [t|TF2 Bool Char|];
            Just tf2Name <- lookupTypeName "TF2";
            testToSimpleExpr testType $ FamilyType tf2Name [ConsType boolType,ConsType charType];
        })
    ];

    testInfoSame :: String -> Info a -> Info b -> TestTree;
    testInfoSame name ia ib = testCase name $ do
    {
        mrefl <- evaluate $ testHetEquality ia ib;
        case mrefl of
        {
            Just ReflH -> return ();
            Nothing -> assertFailure "not equal";
        };
    };

    testReity :: TestTree;
    testReity = testGroup "Reity" [
        testInfoSame "()" (info @()) (info @()),
        testInfoSame "Bool" (info @Bool) (info @Bool),
        testInfoSame "Maybe" (info @Maybe) (info @Maybe),
        testInfoSame "[]" (info @[]) (info @[]),
        let
        {
            im :: Info Maybe;
            im = info;

            ib :: Info Bool;
            ib = info;

            imb :: Info (Maybe Bool);
            imb = applyInfo im ib;
        } in testInfoSame "constructed Maybe Bool" imb imb,
        testInfoSame "Maybe Bool" (info @(Maybe Bool)) (info @(Maybe Bool)),
        testInfoSame "[Bool]" (info @[Bool]) (info @[Bool])
        ];

    tests :: TestTree;
    tests = testGroup "reity" [
        testReity,
        testSimpleType
        ];

    main :: IO ();
    main = defaultMain tests;
}
