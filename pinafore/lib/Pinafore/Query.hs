module Pinafore.Query(QType(..),QValue,QBindings,qdisplay,parseValue) where
{
    import Shapes;
    import Pinafore.Query.Value;
    import Pinafore.Query.Expression;
    import Pinafore.Query.Read;
    import Pinafore.Query.Predefined;


    parseValue :: String -> String -> Result String QValue;
    parseValue name text = do
    {
        expr <- parseExpression name text;
        mval <- qeval $ qlets predefinedBindings expr;
        mval;
    };
}
