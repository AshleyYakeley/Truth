module Main(main) where
{
    import Shapes;
    import Truth.Core;
    import Truth.UI.GTK;
    import Truth.World.Pinafore;
    import Test.Tasty;
    import Test.Tasty.HUnit;


    testGView :: forall edit. Edit edit => UISpec edit -> TestTree;
    testGView uispec = testCase (show uispec) $ case getUIView allGetView getTheView uispec of
    {
        Just _ -> return ();
        Nothing -> assertFailure "not matched";
    };

    data UIUnknown edit where
    {
        MkUIUnknown :: UIUnknown edit;
    };

    instance Show (UIUnknown edit) where
    {
        show MkUIUnknown = "unknown";
    };

    instance UIType UIUnknown where
    {
        uiWitness = $(iowitness [t|UIUnknown|]);
    };

    testGViews :: TestTree;
    testGViews = testGroup "GView" [
            testGView $ MkUISpec $ (MkUIVertical [] :: UIVertical (WholeEdit String)),
            testGView $ MkUISpec $ MkUIVertical [MkUISpec MkUIUnknown :: UISpec (WholeEdit String)],
            testGView $ pinaforeValueSpec rootValue
        ];

    tests :: TestTree;
    tests = testGroup "Truth-GTK" [
        testGViews
        ];

    main :: IO ();
    main = defaultMain tests;
}
