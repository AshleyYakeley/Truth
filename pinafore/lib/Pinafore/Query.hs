module Pinafore.Query(QType(..),QValue,qdisplay,parseValue) where
{
    import Shapes;
    import Pinafore.Query.Value;
    import Pinafore.Query.Expression;
    import Pinafore.Query.Read;


    parseValue :: String -> String -> Result String QValue;
    parseValue name text = do
    {
        expr <- parseExpression name text;
        mval <- qeval expr;
        mval;
    };
}
