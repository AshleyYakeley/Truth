module Main(main) where
{
    import Control.Exception(evaluate);
    import Data.Type.Heterogeneous;
    import Data.Reity;
    import Test.Tasty;
    import Test.Tasty.HUnit;


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

    tests :: TestTree;
    tests = testGroup "reity" [
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

    main :: IO ();
    main = defaultMain tests;
}
