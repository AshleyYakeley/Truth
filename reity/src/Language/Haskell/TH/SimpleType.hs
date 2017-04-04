module Language.Haskell.TH.SimpleType(SimpleType(..),typeToSimple) where
{
    import Language.Haskell.TH as T;


    data SimpleType = VarType Name | AppType SimpleType SimpleType | ConsType Type | FamilyType Name [SimpleType] | EqualType SimpleType SimpleType;

    data Partial a = Complete a | Incomplete (Partial (SimpleType -> a));

    applyPartial :: Partial (SimpleType -> a) -> SimpleType -> Partial a;
    applyPartial (Complete ta) t = Complete $ ta t;
    applyPartial (Incomplete p) t = Incomplete $ applyPartial p t;

    partialFromList :: [sym] -> ([SimpleType] -> a) -> Partial a;
    partialFromList [] f = Complete $ f [];
    partialFromList (_:ss) f = Incomplete $ partialFromList ss (\tt t -> f $ tt ++ [t]);

    typeToPartial :: Type -> Q (Partial SimpleType);
    typeToPartial (ParensT tp) = typeToPartial tp;
    typeToPartial (SigT tp _) = typeToPartial tp;
    typeToPartial (VarT name) = return $ Complete $ VarType name;
    typeToPartial (AppT t1 t2) = do
    {
        pt1 <- typeToPartial t1;
        st2 <- typeToSimple t2;
        case pt1 of
        {
            Complete st1 -> return $ Complete $ AppType st1 st2;
            Incomplete pt -> return $ applyPartial pt st2;
        };
    };
    typeToPartial (InfixT t1 name t2) = typeToPartial $ AppT (AppT (ConT name) t1) t2;
    typeToPartial (ForallT _ _ _) = fail "forall in type";
    typeToPartial WildCardT = fail "_ in type";
    typeToPartial tp@(ConT name) = do
    {
        ninfo <- reify name;
        case ninfo of
        {
            FamilyI dec _ -> do
            {
                vb <- case dec of
                {
                    OpenTypeFamilyD (TypeFamilyHead _ vb _ _) -> return vb;
                    ClosedTypeFamilyD (TypeFamilyHead _ vb _ _) _ -> return vb;
                    _ -> fail $ "type family not really a type family: " ++ show name;
                };
                return $ partialFromList vb (FamilyType name);
            };
            _ -> return $ Complete $ ConsType tp;
        };
    };
    typeToPartial EqualityT = return $ Incomplete $ Incomplete $ Complete EqualType;
    typeToPartial tp = return $ Complete $ ConsType tp;

    typeToSimple :: Type -> Q SimpleType;
    typeToSimple tp = do
    {
        partial <- typeToPartial tp;
        case partial of
        {
            Complete st -> return st;
            Incomplete _ -> fail $ "incomplete type family application";
        }
    }
}
