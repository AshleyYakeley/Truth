module Pinafore.Query.Expression where
{
    import Shapes;
    --import Truth.Core;
    --import Pinafore.Edit;
    import Pinafore.AsText;
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

    qint :: Int -> QValue;
    qint i = MkAny QLiteral $ toText i;

    qvar :: String -> QExpr QValue;
    qvar name = OpenQExpr name $ ClosedQExpr id;

    qabstract :: String -> QExpr a -> QExpr (QValue -> a);
    qabstract _name (ClosedQExpr a) = ClosedQExpr $ \_ -> a;
    qabstract name (OpenQExpr name' expr) | name == name' = fmap (\vva v -> vva v v) $ qabstract name expr;
    qabstract name (OpenQExpr name' expr) = OpenQExpr name' $ fmap (\vva v1 v2 -> vva v2 v1) $ qabstract name expr;

    qlet :: String -> QValue -> QExpr a -> QExpr a;
    qlet name val expr = fmap (\va -> va val) $ qabstract name expr;

    qeval :: MonadFail m => QExpr a -> m a;
    qeval (ClosedQExpr a) = return a;
    qeval (OpenQExpr name _) = fail $ "undefined: " ++ name;
}
