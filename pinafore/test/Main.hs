{-# OPTIONS -fno-warn-orphans #-}
module Main(main) where
{
    import Shapes;
    import Pinafore;
    import Pinafore.Query.Value;
    import Pinafore.Query.Expression;
    import Test.Tasty;
    import Test.Tasty.HUnit;

    -- for test only
    instance Eq QValue where
    {
        (MkAny QLiteral a1) == (MkAny QLiteral a2) = a1 == a2;
        (MkAny QPoint a1) == (MkAny QPoint a2) = a1 == a2;
        _ == _ = error "QValue: not comparable";
    };

    instance (Show e,Show a) => Show (Result e a) where
    {
        show (SuccessResult a) = "success " ++ show a;
        show (FailureResult e) = "failure " ++ show e;
    };

    testQuery :: (Eq a,Show a) => String -> QExpr a -> Maybe a -> TestTree;
    testQuery name expr expected = testCase name $ assertEqual "result" expected $ qeval expr;

    liftA4 :: Applicative m => m a -> m b -> m c -> m d -> m (a,b,c,d);
    liftA4 r1 r2 r3 r4 = (,,,) <$> r1 <*> r2 <*> r3 <*> r4;

    qint :: Int -> Result String QValue;
    qint  = toQValue;

    testQueries :: TestTree;
    testQueries = testGroup "query"
    [
        testQuery "pure A" (pure "A") (Just "A"),
        testQuery "var a" (qvar "a") Nothing,
        testQuery "let a=1;b=2 in (a,b,a,b)" (qlet "a" (pure $ qint 1) $ qlet "b" (pure $ qint 2) $ liftA4 <$> (qvar "a") <*> (qvar "b") <*> (qvar "a") <*> (qvar "b")) (Just $ liftA4 (qint 1) (qint 2) (qint 1) (qint 2)),
        testQuery "let a=1;b=2 in (b,a,b,a)" (qlet "a" (pure $ qint 1) $ qlet "b" (pure $ qint 2) $ liftA4 <$> (qvar "b") <*> (qvar "a") <*> (qvar "b") <*> (qvar "a")) (Just $ liftA4 (qint 2) (qint 1) (qint 2) (qint 1))
    ];

    tests :: TestTree;
    tests = testGroup "pinafore"
    [
        testQueries
    ];

    main :: IO ();
    main = defaultMain tests;
}
