module Data.Reity.Template
(
    generateTypeName,
    generateTypeKnowledge,
    generateTypeKnowledgeFromType,
    generateTypeInfoExpr,
    generateTypeMatchExpr,
    generateFamilyProxy,
    generateShowSimpleTypeFail
) where
{
    import Data.List;
    import Data.Maybe;
    import Data.Foldable;
    import Data.Traversable;
    import Language.Haskell.TH as T;
    import qualified Data.Type.Heterogeneous as R;
    import Control.Monad.Trans.Class;
    import Control.Monad.Trans.Writer;
    import qualified Data.Knowledge as R;
    import qualified Data.Reity.Wit as R;
    import qualified Data.Reity.TypeInfo as R;
    import qualified Data.Reity.Match as R;
    import Language.Haskell.TH.SimpleType;


    generateTypeName :: TypeQ -> Q Exp;
    generateTypeName qtp = do
    {
        tp <- qtp;
        let
        {
            tpname = case tp of
            {
                ConT n -> nameBase n;
                VarT n -> nameBase n;
                PromotedT n -> nameBase n;
                _ -> show tp;
            };
        };
        [|tpname|];
    };

    type M = WriterT [Stmt] Q;

    write :: Stmt -> M ();
    write stmt = tell [stmt];

    writeQ :: Q Stmt -> M ();
    writeQ qstmt = do
    {
        stmt <- lift qstmt;
        write stmt;
    };

    declFail :: String -> Q a;
    declFail s = fail $ "generateTypeKnowledge: " ++ s;

    vfNames :: Name -> (Name,Name);
    vfNames name = let
    {
        typeName = nameBase name ++ "TypeInfo";
        constructorName = "Mk" ++ typeName;
    } in (mkName typeName,mkName constructorName);

    deconstructBound :: Name -> SimpleType -> M ([Name],[(Name,Name)]);
    deconstructBound subjectN (VarType typeN) = return ([],[(typeN,subjectN)]);
    deconstructBound subjectN (AppType tp1 tp2) = do
    {
        sN1 <- lift $ newName "_f";
        sN2 <- lift $ newName "_a";
        writeQ $ bindS [p|R.MkSplitTypeInfo $(varP sN1) $(varP sN2)|] [e|matchTypeInfo $(varE subjectN)|];
        vars1 <- deconstruct sN1 tp1;
        vars2 <- deconstruct sN2 tp2;
        return $ ([],vars1 ++ vars2);
    };
    deconstructBound subjectN (ConsType tp) = do
    {
        vars <- case tp of
        {
            SigT _ ktp -> do
            {
                kstp <- lift $ typeToSimple ktp;
                kindN <- lift $ newName "_k";
                expr <- lift [e|R.typeInfoKind $(varE subjectN)|];
                writeQ $ return $ LetS [ValD (VarP kindN) (NormalB expr) []];
                deconstruct kindN kstp;
            };
            _ -> return [];
        };
        writeQ $ bindS [p|R.ReflH|] [e|R.sameTypeInfo (R.typeInfo :: R.TypeInfo $(return tp)) $(varE subjectN)|];
        return ([],vars);
    };
    deconstructBound subjectN (FamilyType name []) = do
    {
        let
        {
            (typeN,_) = vfNames name;
        };
        writeQ $ bindS [p|R.ReflH|] [e|R.sameTypeInfo (R.typeInfo :: R.TypeInfo $(conT typeN)) $(varE subjectN)|];
        return ([],[]);
    };
    deconstructBound subjectN (FamilyType name args) = let
    {
        mkST t [] = t;
        mkST t (a:aa) = mkST (AppType t a) aa;
    } in do
    {
        free <- deconstruct subjectN $ mkST (FamilyType name []) args;
        return ([],free);
    };
    deconstructBound _ (EqualType _ _) = lift $ declFail "equality not allowed in instance";
    deconstructBound subjectN (ForAllType vars ctxt body) = do
    {
        (oldbound,oldfree) <- deconstructBound subjectN body;
        matchUpVars oldfree;
        matchContext oldfree ctxt;
        let
        {
            newbound = catMaybes $ fmap (\v -> lookup v oldfree) vars;
            newfree = [ ts | ts@(t,_) <- oldfree, not $ elem t vars];
        };
        return (newbound ++ oldbound,newfree);
    };

    deconstruct :: Name -> SimpleType -> M [(Name,Name)];
    deconstruct subjectN st = do
    {
        (bound,free) <- deconstructBound subjectN st;
        case bound of
        {
            [] -> return free;
            _ -> lift $ declFail "forall not allowed in expression";
        };
    };

    constructInfoBoundExpr :: (?varMap :: [(Name,Name)]) =>
        SimpleType -> M ([Name],Exp);
    constructInfoBoundExpr (AppType tp1 tp2) = do
    {
        exp1 <- constructInfoExpr tp1;
        exp2 <- constructInfoExpr tp2;
        expr <- lift $ [e|R.applyTypeInfo $(return exp1) $(return exp2)|];
        return ([],expr);
    };
    constructInfoBoundExpr (VarType typeN) = case lookup typeN ?varMap of
    {
        Just subjectN -> return $ ([],VarE subjectN);
        Nothing -> lift $ declFail $ "type variable not found: " ++ show typeN;
    };
    constructInfoBoundExpr (ConsType tp) = do
    {
        expr <- lift [e|R.typeInfo :: R.TypeInfo $(return $ tp)|];
        return ([],expr);
    };
    constructInfoBoundExpr (FamilyType name vc) = do
    {
        let
        {
            (typeN,constructorN) = vfNames name;

            typeFamilyInfoExpr :: Exp -> [SimpleType] -> M Exp;
            typeFamilyInfoExpr expr [] = return expr;
            typeFamilyInfoExpr expr (t:tt) = do
            {
                texp <- constructInfoExpr t;
                exptexp <- lift $ [e|R.applyTypeInfo $(return expr) $(return texp)|];
                typeFamilyInfoExpr exptexp tt;
            };
        };
        tfTypeExpr <- lift [e|R.typeInfo :: R.TypeInfo $(return $ ConT typeN)|];
        infoExpr <- typeFamilyInfoExpr tfTypeExpr vc;
        resultN <- lift $ newName "_var";
        writeQ $ bindS [p|R.ValueFact $(conP constructorN [varP resultN])|] [e|R.askTypeInfo $(return infoExpr)|];
        return $ ([],VarE resultN);
    };
    constructInfoBoundExpr (EqualType _ _) = lift $ declFail "equality not allowed in context expression";
    constructInfoBoundExpr (ForAllType vars [] body) = do
    {
        pairs <- for vars $ \varN -> do
        {
            argN <- lift $ newName "_arg";
            return (varN,argN);
        };
        nbody <- let ?varMap = ?varMap ++ pairs in constructInfoExpr body;
        return $ (fmap snd pairs,nbody);
    };
    constructInfoBoundExpr (ForAllType _ _ _) = lift $ declFail "context in forall not supported";

    constructInfoExpr :: (?varMap :: [(Name,Name)]) =>
        SimpleType -> M Exp;
    constructInfoExpr st = do
    {
        (names,expr) <- constructInfoBoundExpr st;
        case names of
        {
            [] -> return expr;
            _ -> lift $ declFail "forall not allowed in expression";
        }
    };

    matchContextType :: (?varMap :: [(Name,Name)]) =>
        SimpleType -> M ();
    matchContextType (EqualType st1 st2) = do
    {
        info1 <- constructInfoExpr st1;
        info2 <- constructInfoExpr st2;
        writeQ $ bindS [p|R.ReflH|] [e|R.sameTypeInfo $(return info1) $(return info2)|];
    };
    matchContextType st = do
    {
        infoExpr <- constructInfoExpr st;
        writeQ $ bindS [p|R.ConstraintFact|] [e|R.askTypeInfo $(return infoExpr)|];
    };

    matchContext :: [(Name,Name)] -> [SimpleType] -> M ();
    matchContext varMap ctxt = let {?varMap = varMap} in traverse_ matchContextType ctxt;

    -- vars used more than once need to be matched
    matchUpVar :: (Name,Name) -> [(Name, Name)] -> M ();
    matchUpVar (typeN,subjectN) varMap = case lookup typeN varMap of
    {
        Just subjectN' -> writeQ $ bindS [p|R.ReflH|] [e|R.sameTypeInfo $(return $ VarE subjectN) $(return $ VarE subjectN')|];
        Nothing -> return ();
    };

    matchUpVars :: [(Name, Name)] -> M ();
    matchUpVars [] = return ();
    matchUpVars (t:tt) = do
    {
        matchUpVar t tt;
        matchUpVars tt;
    };

    instanceStmts :: [Type] -> Type -> Name -> M ();
    instanceStmts ctxt tp subjectN = do
    {
        sc <- for ctxt $ \t -> lift $ typeToSimple t;
        st <- lift $ typeToSimple tp;
        varMap <- deconstruct subjectN st;
        matchUpVars varMap;
        matchContext varMap sc;
        matchStmtE <- lift [e|return R.ConstraintFact|];
        write $ NoBindS matchStmtE;
    };

    assocTypeKnowledgeE :: Dec -> Q (Maybe (Name,Exp));
    assocTypeKnowledgeE (TySynInstD patName (TySynEqn patArgsTs exprT)) = fmap (\expr -> Just (patName,expr)) $ stmtsToKnowledgeE $ \subjectN -> do
    {
        patArgsSTs <- lift $ traverse typeToSimple patArgsTs;
        varMap <- deconstruct subjectN $ FamilyType patName patArgsSTs;
        exprST <- lift $ typeToSimple exprT;
        infoExpr <- let {?varMap = varMap} in constructInfoExpr exprST;
        let
        {
            (_,constructorN) = vfNames patName;
        };
        writeQ $ noBindS $ [e|return $ R.ValueFact ($(conE constructorN) $(return infoExpr))|];
    };
    assocTypeKnowledgeE _ = return Nothing;

    stmtsToKnowledgeE :: (Name -> M ()) -> Q Exp;
    stmtsToKnowledgeE getM = do
    {
        subjectN <- newName "_subject";
        stmts <- execWriterT $ getM subjectN;
        [e|R.MkKnowledge $ \ $(varP subjectN) -> $(return $ DoE stmts)|];
    };

    getClassAssociatedTypes :: Name -> Q [Name];
    getClassAssociatedTypes className = do
    {
        classInfo <- reify className;
        classDecs <- case classInfo of
        {
            (ClassI (ClassD _ _ _ _ decs) _) -> return decs;
            _ -> fail $ "not a class: " ++ show className;
        };
        let
        {
            decATName (OpenTypeFamilyD (TypeFamilyHead name _ _ _)) = Just name;
            decATName (ClosedTypeFamilyD (TypeFamilyHead name _ _ _) _) = Just name;
            decATName _ = Nothing;
        };
        return $ mapMaybe decATName classDecs;
    };

    instanceKnowledgeEs :: [Type] -> Type -> [Dec] -> Q [Exp];
    instanceKnowledgeEs ctxt tp decls = do
    {
        (className,_) <- headForm tp;
        classAssocTypes <- getClassAssociatedTypes className;
        instE <- stmtsToKnowledgeE $ instanceStmts ctxt tp;
        massocTypeEs <- traverse assocTypeKnowledgeE decls;
        let
        {
            assocTypeEs = catMaybes massocTypeEs;
            assocTypeNs = fmap fst assocTypeEs;
        };
        for_ (classAssocTypes \\ assocTypeNs) $ \name -> reportWarning $ show className ++ " is missing definition for " ++ nameBase name;
        for_ (assocTypeNs \\ classAssocTypes) $ \name -> reportWarning $ show className ++ " has unexpected definition " ++ nameBase name;
        return $ instE : (fmap snd assocTypeEs);
    };

    decKnowledgeEs :: Dec -> Q [Exp];
    decKnowledgeEs (InstanceD _ ctxt tp decls) = instanceKnowledgeEs ctxt tp decls;
    decKnowledgeEs decl = do
    {
        mne <- assocTypeKnowledgeE decl;
        case mne of
        {
            Just (_name,expr) -> return [expr];
            Nothing -> declFail $ "non-instance declaration: " ++ show decl;
        };
    };

    generateTypeKnowledge :: Q [Dec] -> Q Exp;
    generateTypeKnowledge qds = do
    {
        ds <- qds;
        exprs <- traverse decKnowledgeEs ds;
        [e|mconcat $(return $ ListE $ mconcat exprs)|];
    };

    headForm :: Type -> Q (Name,[Type]);
    headForm (ParensT t) = headForm t;
    headForm (SigT t _) = headForm t;
    headForm (ConT n) = return (n,[]);
    headForm (InfixT t1 n t2) = return (n,[t1,t2]);
    headForm (UInfixT t1 n t2) = return (n,[t1,t2]);
    headForm (AppT t1 t2) = do
    {
        (h,args) <- headForm t1;
        return (h,args ++ [t2]);
    };
    headForm t = declFail $ "not a class: " ++ show t;

    generateTypeKnowledgeFromType :: Q Type -> Q Exp;
    generateTypeKnowledgeFromType qtp = do
    {
        tp <- qtp;
        (name,args) <- headForm tp;
        generateTypeKnowledge $ reifyInstances name args;
    };


    lambdaBind :: [Name] -> Exp -> Exp;
    lambdaBind [] expr = expr;
    lambdaBind vv expr = LamE (fmap VarP vv) expr;

    -- forall a b. Context a b => T a b
    -- forall a b. TypeInfo a -> TypeInfo b -> ReasonM (TypeInfo (T a b))
    generateTypeInfoExpr :: Q Type -> Q Exp;
    generateTypeInfoExpr qtp = do
    {
        tp <- qtp;
        st <- typeToSimple tp;
        (names,stmts) <- runWriterT $ do
        {
            (names,expr) <- let ?varMap = [] in constructInfoBoundExpr st;
            writeQ $ noBindS [e|return $(return expr)|];
            return names;
        };
        return $ lambdaBind names $ DoE stmts;
    };

    applyExprs :: Exp -> [Exp] -> Exp;
    applyExprs e [] = e;
    applyExprs e (a:aa) = applyExprs (AppE e a) aa;

    generateTypeMatchExpr :: Q Type -> Q Exp -> Q Exp;
    generateTypeMatchExpr qtp qexpr = do
    {
        tp <- qtp;
        expr <- qexpr;
        st <- typeToSimple tp;
        subjectN <- newName "_subj";
        ((),stmts) <- runWriterT $ do
        {
            (bound,free) <- deconstructBound subjectN st;
            case free of
            {
                [] -> writeQ $ noBindS $ return $ applyExprs expr $ fmap VarE bound;
                _ -> lift $ declFail $ "free variables: " ++ (intercalate " "  $ nub $ fmap (nameBase . fst) free);
            };
        };
        return $ lambdaBind [subjectN] $ DoE stmts;
    };

    typeFamilyProxyDec :: Name -> Q [Dec];
    typeFamilyProxyDec famName = do
    {
        famInfo <- reify famName;
        TypeFamilyHead _ argBndrs _ _ <- case famInfo of
        {
            FamilyI famDec _ -> case famDec of
            {
                OpenTypeFamilyD decHead -> return decHead;
                ClosedTypeFamilyD decHead _ -> return decHead;
                _ -> fail $ show famName ++ " is not a proper type family";
            };
            _ -> fail $ show famName ++ " is not a type family";
        };
        argNames <- traverse (\_ -> newName "a") argBndrs;
        infoType <- [t|R.TypeInfo|];
        let
        {
            famType :: Type;
            famType = ConT famName;

            (typeName,constructorName) = vfNames famName;

            argTypes :: [Type];
            argTypes = fmap VarT argNames;

            argTyVarBndrs :: [TyVarBndr];
            argTyVarBndrs = fmap PlainTV argNames;

            appTypes :: Type -> [Type] -> Type;
            appTypes base [] = base;
            appTypes base (t:tt) = appTypes (AppT base t) tt;

            con :: Con;
            con = NormalC constructorName [(Bang NoSourceUnpackedness NoSourceStrictness,AppT infoType $ appTypes famType argTypes)];

            typeDec :: Dec;
            typeDec = NewtypeD [] typeName argTyVarBndrs Nothing con [];

            typeTypeQ :: TypeQ;
            typeTypeQ = return $ ConT typeName;
        };
        instDecs <- [d|
            instance R.HasTypeInfo $(typeTypeQ) where
            {
                typeWitness = $(R.generateWitness typeTypeQ);
                typeName _ = $(generateTypeName typeTypeQ);
            };
        |];
        return $ typeDec : instDecs;
    };

    generateFamilyProxy :: String -> Q [Dec];
    generateFamilyProxy famNameStr = typeFamilyProxyDec $ mkName famNameStr;
}
