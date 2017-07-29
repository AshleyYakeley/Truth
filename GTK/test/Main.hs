{-# OPTIONS -fno-warn-orphans #-}
module Main(main) where
{
    import Data.Kind;
    import Data.Result;
    import Data.Reity;
    import Truth.Core;
    import Truth.World.Soup;
    import Graphics.UI.Gtk;
    import Truth.UI.GTK;
    import Test.Tasty;
    import Test.Tasty.HUnit;


    testGView :: forall (edit :: Type). TypeInfo edit -> TestTree;
    testGView iedit = testCase (show iedit) $ case runKnowledge (mappend allKnowledge $ typeInfoKnowledge iedit) $ findView @Widget iedit of
    {
        SuccessResult _ -> return ();
        FailureResult fr -> assertFailure $ show fr;
    };

    testGViews :: TestTree;
    testGViews = testGroup "GView" [
            testGView $ typeInfo @(WholeEdit Bool),
            testGView $ typeInfo @(StringEdit String),
            testGView $ typeInfo @(SoupEdit (MutableIOEdit ByteStringEdit)),
            testGView $ typeInfo @(SumWholeEdit (OneEdit (Result String) (WholeEdit Bool))),
            testGView $ typeInfo @(SumWholeEdit (OneEdit (Result String) (StringEdit String)))
        ];

    tests :: TestTree;
    tests = testGroup "Truth-GTK" [
        testGViews
        ];

    main :: IO ();
    main = defaultMain tests;
}
