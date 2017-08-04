module Main(main) where
{
    import Prelude;
    import Control.Exception(evaluate);
    import Data.Type.Heterogeneous;
    import Language.Haskell.TH;
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

    testInfoSame :: String -> TypeInfo a -> TypeInfo b -> TestTree;
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
        testInfoSame "()" (typeInfo @()) (typeInfo @()),
        testInfoSame "Bool" (typeInfo @Bool) (typeInfo @Bool),
        testInfoSame "Maybe" (typeInfo @Maybe) (typeInfo @Maybe),
        testInfoSame "[]" (typeInfo @[]) (typeInfo @[]),
        let
        {
            im :: TypeInfo Maybe;
            im = typeInfo;

            ib :: TypeInfo Bool;
            ib = typeInfo;

            imb :: TypeInfo (Maybe Bool);
            imb = applyTypeInfo im ib;
        } in testInfoSame "constructed Maybe Bool" imb imb,
        testInfoSame "Maybe Bool" (typeInfo @(Maybe Bool)) (typeInfo @(Maybe Bool)),
        testInfoSame "[Bool]" (typeInfo @[Bool]) (typeInfo @[Bool])
        ];

    tests :: TestTree;
    tests = testGroup "reity" [
        testReity,
        testSimpleType
        ];

    main :: IO ();
    main = defaultMain tests;
}
