module Pinafore.Query.Expression where
{
    import Shapes;
    import Pinafore.Query.Value;


    data QExpr a = ClosedQExpr a | OpenQExpr String (QExpr (QValue -> a));

    instance Functor QExpr where
    {
        fmap ab (ClosedQExpr a) = ClosedQExpr $ ab a;
        fmap ab (OpenQExpr name expr) = OpenQExpr name $ fmap (\va v -> ab $ va v) expr;
    };

    instance Applicative QExpr where
    {
        pure = ClosedQExpr;
        (ClosedQExpr ab) <*> expr = fmap ab expr;
        (OpenQExpr name exprab) <*> expr = OpenQExpr name $ (\vab a v -> vab v a) <$> exprab <*> expr;
    };

    qabstract :: String -> QExpr a -> QExpr (QValue -> a);
    qabstract _name (ClosedQExpr a) = ClosedQExpr $ \_ -> a;
    qabstract name (OpenQExpr name' expr) | name == name' = fmap (\vva v -> vva v v) $ qabstract name expr;
    qabstract name (OpenQExpr name' expr) = OpenQExpr name' $ fmap (\vva v1 v2 -> vva v2 v1) $ qabstract name expr;

    qeval :: MonadFail m => QExpr a -> m a;
    qeval (ClosedQExpr a) = return a;
    qeval (OpenQExpr name _) = fail $ "undefined: " ++ name;

    type QValueExpr = QExpr (Result String QValue);

    exprAbstract :: String -> QValueExpr -> QValueExpr;
    exprAbstract name expr = fmap (return . qfunction) $ qabstract name expr;

    exprAbstracts :: [String] -> QValueExpr -> QValueExpr;
    exprAbstracts [] = id;
    exprAbstracts (n:nn) = exprAbstract n . exprAbstracts nn;

    qvar :: String -> QValueExpr;
    qvar name = OpenQExpr name $ ClosedQExpr return;

    qlet :: String -> QValueExpr -> QExpr (Result String a) -> QExpr (Result String a);
    qlet name val body = (>>=) <$> val <*> qabstract name body;

    newtype QBindings = MkQBindings [(String,QValueExpr)] deriving (Semigroup,Monoid);

    qlets :: QBindings -> QExpr (Result String a) -> QExpr (Result String a);
    qlets = let
    {
        qlets' [] = id;
        qlets' ((n,v):bb) = qlet n v . qlets' bb;
    } in \(MkQBindings bb) -> qlets' bb;

    exprApply :: QValueExpr-> QValueExpr -> QValueExpr;
    exprApply = liftA2 $ \mf ma -> do
    {
        f <- mf;
        a <- ma;
        qapply f a;
    };

    exprApplyAll :: QValueExpr -> [QValueExpr] -> QValueExpr;
    exprApplyAll e [] = e;
    exprApplyAll e (a:aa) = exprApplyAll (exprApply e a) aa;
}
