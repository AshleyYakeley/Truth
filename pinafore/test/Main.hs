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
        _ == _ = error "QValue: not comparable";
    };

    instance (Show e,Show a) => Show (Result e a) where
    {
        show (SuccessResult a) = "success " ++ show a;
        show (FailureResult e) = "failure " ++ show e;
    };

    testQueryValue :: (Eq a,Show a) => String -> QExpr a -> Maybe a -> TestTree;
    testQueryValue name expr expected = testCase name $ assertEqual "result" expected $ qeval expr;

    liftA4 :: Applicative m => m a -> m b -> m c -> m d -> m (a,b,c,d);
    liftA4 r1 r2 r3 r4 = (,,,) <$> r1 <*> r2 <*> r3 <*> r4;

    qint :: Int -> Result String QValue;
    qint  = toQValue;

    testQueryValues :: TestTree;
    testQueryValues = testGroup "query values"
    [
        testQueryValue "pure A" (pure "A") (Just "A"),
        testQueryValue "var a" (qvar "a") Nothing,
        testQueryValue "let a=1;b=2 in (a,b,a,b)" (qlet "a" (pure $ qint 1) $ qlet "b" (pure $ qint 2) $ liftA4 <$> (qvar "a") <*> (qvar "b") <*> (qvar "a") <*> (qvar "b")) (Just $ liftA4 (qint 1) (qint 2) (qint 1) (qint 2)),
        testQueryValue "let a=1;b=2 in (b,a,b,a)" (qlet "a" (pure $ qint 1) $ qlet "b" (pure $ qint 2) $ liftA4 <$> (qvar "b") <*> (qvar "a") <*> (qvar "b") <*> (qvar "a")) (Just $ liftA4 (qint 2) (qint 1) (qint 2) (qint 1))
    ];

    testQuery :: String -> Maybe String -> TestTree;
    testQuery query expected = testCase query $ case (expected,parseValue "<input>" query) of
    {
        (Nothing,FailureResult _) -> return ();
        (Nothing,SuccessResult v) -> assertFailure $ "expected failure, found success: " ++ show v;
        (Just _,FailureResult e) -> assertFailure $ "expected success, found failure: " ++ e;
        (Just s,SuccessResult (v :: QValue)) -> assertEqual "result" s (show v);
    };

    testQueries :: TestTree;
    testQueries = testGroup "queries"
    [
        testQuery "" $ Nothing,
        testQuery "x" $ Nothing,

        -- constants
        testQuery "\"\"" $ Just "",
        testQuery "\"Hello \"" $ Just "Hello ",
        testQuery "true" $ Just "true",
        testQuery "false" $ Just "false",
        testQuery "\"1\"" $ Just "1",
        testQuery "3" $ Just "3",

        -- list construction
        testQuery "[]" $ Just "[]",
        testQuery "[1]" $ Just "[1]",
        testQuery "[1,2,3]" $ Just "[1,2,3]",

        -- let-binding
        testQuery "let a=\"5\" in a" $ Just "5",
        testQuery "let a=5 in a" $ Just "5",
        testQuery "let a=1;b=2 in a" $ Just "1",
        testQuery "let a=1;b=2 in b" $ Just "2",
        testQuery "let a=1;b=2 in b" $ Just "2",
        testQuery "let a=1;b=\"2\" in b" $ Just "2",
        testQuery "let a=1 ;b=\"2\" in b" $ Just "2",
        testQuery "let a= 1 ;b=\"2\" in b" $ Just "2",
        testQuery "let a=7;b=a in a" $ Just "7",
        testQuery "let a=7;b=a in b" $ Just "7",
        testQuery "let a=2 in let b=a in b" $ Just "2",

        -- lexical scoping
        testQuery "let a=1 in let b=a in let a=3 in a" $ Just "3",
        testQuery "let a=1;b=a;a=3 in a" $ Just "3",
        testQuery "let a=1 in let b=a in let a=3 in b" $ Just "1",
        testQuery "let a=1;b=a;a=3 in b" $ Just "1"
    ];

    tests :: TestTree;
    tests = testGroup "pinafore"
    [
        testQueryValues,
        testQueries
    ];

    main :: IO ();
    main = defaultMain tests;
}
