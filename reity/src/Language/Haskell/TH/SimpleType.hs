module Language.Haskell.TH.SimpleType(SimpleType(..),typeToSimple,generateShowSimpleTypeFail) where
{
    import Data.List(intercalate);
    import Data.Traversable(for);
    import Language.Haskell.TH as T;


    data SimpleType = ForAllType [Name] [SimpleType] SimpleType | VarType Name | AppType SimpleType SimpleType | ConsType Type | FamilyType Name [SimpleType] | EqualType SimpleType SimpleType deriving (Eq);

    showSingle :: SimpleType -> String;
    showSingle t@(AppType _ _) = "(" ++ show t ++ ")";
    showSingle t@(FamilyType _ _) = "(" ++ show t ++ ")";
    showSingle t@(EqualType _ _) = "(" ++ show t ++ ")";
    showSingle t = show t;

    instance Show SimpleType where
    {
        show (ForAllType names ctxt t) = "forall " ++ intercalate " " (fmap nameBase names) ++ ". (" ++ intercalate "," (fmap show ctxt) ++ ") => " ++ show t;
        show (VarType name) = nameBase name;
        show (AppType t1 t2) = show t1 ++ " " ++ showSingle t2;
        show (ConsType (ConT name)) = nameBase name;
        show (ConsType t) = show t;
        show (FamilyType name ts) = show name ++ " " ++ intercalate " " (fmap showSingle ts);
        show (EqualType t1 t2) = show t1 ++ " ~ " ++ show t2;
    };

    data Partial a = Complete a | Incomplete (Partial (SimpleType -> a));

    instance Functor Partial where
    {
        fmap ab (Complete a) = Complete $ ab a;
        fmap ab (Incomplete psa) = Incomplete $ fmap (\sa -> ab . sa) psa;
    };

    applyPartial :: Partial (t -> a) -> t -> Partial a;
    applyPartial pta t = fmap (\ta -> ta t) pta;

    partialFromList :: [sym] -> ([(sym,SimpleType)] -> a) -> Partial a;
    partialFromList [] f = Complete $ f [];
    partialFromList (s:ss) f = Incomplete $ partialFromList ss (\tt t -> f $ [(s,t)] ++ tt);

    substitute :: (?names :: [(Name,SimpleType)]) => SimpleType -> SimpleType;
    substitute (VarType name) | Just val <- lookup name ?names = val;
    substitute (AppType t1 t2) = AppType (substitute t1) (substitute t2);
    substitute (FamilyType name args) = FamilyType name $ fmap substitute args;
    substitute (EqualType t1 t2) = EqualType (substitute t1) (substitute t2);
    substitute t = t;

    getTVName :: TyVarBndr -> Name;
    getTVName (PlainTV n) = n;
    getTVName (KindedTV n _) = n;

    consSimpleType :: Maybe Kind -> Type -> SimpleType;
    consSimpleType Nothing t = ConsType t;
    consSimpleType (Just k) t = ConsType $ SigT t k;

    tvbToName :: TyVarBndr -> Q Name;
    tvbToName (PlainTV n) = return n;
    tvbToName (KindedTV n _) = return n;

    typeToPartial :: Maybe Kind -> Type -> Q (Partial SimpleType);
    typeToPartial mk (ParensT tp) = typeToPartial mk tp;
    typeToPartial _ (SigT tp k) = typeToPartial (Just k) tp;
    typeToPartial _ (VarT name) = return $ Complete $ VarType name;
    typeToPartial _ (AppT t1 t2) = do
    {
        pt1 <- typeToPartial Nothing t1;
        st2 <- typeToSimple t2;
        case pt1 of
        {
            Complete st1 -> return $ Complete $ AppType st1 st2;
            Incomplete pt -> return $ applyPartial pt st2;
        };
    };
    typeToPartial mk (InfixT t1 name t2) = typeToPartial mk $ AppT (AppT (ConT name) t1) t2;
    typeToPartial _ (ForallT tvb ctxt body) = do
    {
        snames <- for tvb tvbToName;
        sctxt <- for ctxt typeToSimple;
        sbody <- typeToSimple body;
        return $ Complete $ ForAllType snames sctxt sbody;
    };
    typeToPartial _ WildCardT = fail "_ in type";
    typeToPartial mk tp@(ConT name) = do
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
                return $ partialFromList vb (\args -> FamilyType name $ fmap snd args);
            };
            TyConI (TySynD _ arglist defn) -> do
            {
                sdefn <- typeToSimple defn;
                return $ partialFromList (fmap getTVName arglist) $ \argmap -> let {?names = argmap} in substitute sdefn;
            };
            TyConI _ -> return $ Complete $ consSimpleType mk tp;
            ClassI _ _ -> return $ Complete $ consSimpleType mk tp;
            _ -> fail $ show name ++ ": unknown typeInfo for type: " ++ show ninfo;
        };
    };
    typeToPartial _ EqualityT = return $ Incomplete $ Incomplete $ Complete EqualType;
    typeToPartial mk tp = return $ Complete $ consSimpleType mk tp;

    -- | Convert 'Type' to the more wieldy 'SimpleType'. Expands type synonyms, identifies type families and type equalities.
    typeToSimple :: Type -> Q SimpleType;
    typeToSimple tp = do
    {
        partial <- typeToPartial Nothing tp;
        case partial of
        {
            Complete st -> return st;
            Incomplete _ -> fail $ "incomplete type synonym application";
        };
    };

    generateShowSimpleTypeFail :: Q Type -> Q [Dec];
    generateShowSimpleTypeFail qt = do
    {
        t <- qt;
        st <- typeToSimple t;
        fail $ show st;
    };
}
