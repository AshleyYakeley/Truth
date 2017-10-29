{-# OPTIONS -fno-warn-orphans #-}
module Main(main) where
{
    import Shapes;
    import Pinafore;
    import Test.Tasty;
    import Test.Tasty.HUnit;

    -- for test only
    instance Eq QValue where
    {
        (MkAny (QPrimitive (QLiteral QInt)) a1) == (MkAny (QPrimitive (QLiteral QInt)) a2) = a1 == a2;
        _ == _ = error "not Int";
    };

    testQuery :: (Eq a,Show a) => String -> QExpr a -> Maybe a -> TestTree;
    testQuery name expr expected = testCase name $ assertEqual "result" expected $ qeval expr;

    testQueries :: TestTree;
    testQueries = testGroup "query"
    [
        testQuery "pure A" (pure "A") (Just "A"),
        testQuery "var a" (qvar "a") Nothing,
        testQuery "let a=1;b=2 in (a,b,a,b)" (qlet "a" (qint 1) $ qlet "b" (qint 2) $ (,,,) <$> (qvar "a") <*> (qvar "b") <*> (qvar "a") <*> (qvar "b")) (Just (qint 1,qint 2,qint 1,qint 2)),
        testQuery "let a=1;b=2 in (b,a,b,a)" (qlet "a" (qint 1) $ qlet "b" (qint 2) $ (,,,) <$> (qvar "b") <*> (qvar "a") <*> (qvar "b") <*> (qvar "a")) (Just (qint 2,qint 1,qint 2,qint 1))
    ];

    tests :: TestTree;
    tests = testGroup "pinafore"
    [
        testQueries
    ];

    main :: IO ();
    main = defaultMain tests;
}