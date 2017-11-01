module Pinafore.Query(QType(..),QValue,FromQValue(..),QBindings,qdisplay,parseValue) where
{
    import Shapes;
    import Pinafore.Query.Value;
    import Pinafore.Query.Expression;
    import Pinafore.Query.Read;
    import Pinafore.Query.Predefined;


    parseValue :: FromQValue t => String -> String -> Result String t;
    parseValue name text = do
    {
        expr <- parseExpression name text;
        mval <- qeval $ qlets predefinedBindings expr;
        val <- mval;
        fromQValue val;
    };
}
