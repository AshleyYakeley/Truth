module SimpleType where
{
    import Language.Haskell.TH;
    import Language.Haskell.TH.Syntax;
    import Language.Haskell.TH.SimpleType;


    testToSimpleExpr :: Type -> SimpleType -> Q Exp;
    testToSimpleExpr t expected = do
    {
        found <- typeToSimple t;
        let
        {
            r :: Maybe (String,String);
            r = if found == expected then Nothing else Just (show expected,show found);
        };
        lift (show t,r);
    };
}
