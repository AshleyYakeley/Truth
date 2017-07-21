{-# OPTIONS -fno-warn-orphans #-}
module Main(main) where
{
    import Data.Kind;
    import Data.Result;
    import Data.Reity;
    import Truth.Core;
    import Truth.World.Soup;
    import Truth.UI.GTK;
    import Test.Tasty;
    import Test.Tasty.HUnit;


    testGView :: forall (edit :: Type). Info edit -> TestTree;
    testGView iedit = testCase (show iedit) $ case getGView matchViews iedit of
    {
        SuccessResult _ -> return ();
        FailureResult fr -> assertFailure $ show fr;
    };

    testGViews :: TestTree;
    testGViews = testGroup "GView" [
            testGView $ info @(WholeEdit Bool),
            testGView $ info @(StringEdit String),
            testGView $ info @(SoupEdit (ObjectEdit ByteStringEdit))
        ];

    tests :: TestTree;
    tests = testGroup "Truth-GTK" [
        testGViews
        ];

    main :: IO ();
    main = defaultMain tests;
}
