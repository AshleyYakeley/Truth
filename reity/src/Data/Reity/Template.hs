module Data.Reity.Template(declInfo,instInfo) where
{
    import Data.Foldable;
    import Language.Haskell.TH as T;
    import Data.Type.Heterogeneous;
    import Control.Monad.Trans.Class;
    import Control.Monad.Trans.Writer;
    import Data.Knowledge;
    import qualified Data.Reity.Info as R;
    import qualified Data.Reity.Match as R;
    import Language.Haskell.TH.SimpleType;


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
    declFail s = fail $ "declInfo: " ++ s;

    vfNames :: Name -> (Name,Name);
    vfNames name = let
    {
        typeName = nameBase name ++ "Info";
        constructorName = "Mk" ++ typeName;
    } in (mkName typeName,mkName constructorName);

    deconstruct :: Name -> SimpleType -> M [(Name,Name)];
    deconstruct subjectN (VarType typeN) = return [(typeN,subjectN)];
    deconstruct subjectN (AppType tp1 tp2) = do
    {
        sN1 <- lift $ newName "_f";
        sN2 <- lift $ newName "_a";
        writeQ $ bindS [p|R.MkSplitInfo $(varP sN1) $(varP sN2)|] [e|matchInfo $(varE subjectN)|];
        vars1 <- deconstruct sN1 tp1;
        vars2 <- deconstruct sN2 tp2;
        return $ vars1 ++ vars2;
    };
    deconstruct subjectN (ConsType tp) = do
    {
        writeQ $ bindS [p|ReflH|] [e|testHetEquality (info :: R.Info $(return tp)) $(varE subjectN)|];
        return [];
    };
    deconstruct subjectN (FamilyType name []) = do
    {
        let
        {
            (typeN,_) = vfNames name;
        };
        writeQ $ bindS [p|ReflH|] [e|testHetEquality (info :: R.Info $(conT typeN)) $(varE subjectN)|];
        return [];
    };
    deconstruct subjectN (FamilyType name args) = let
    {
        mkST t [] = t;
        mkST t (a:aa) = mkST (AppType t a) aa;
    } in deconstruct subjectN $ mkST (FamilyType name []) args;
    deconstruct _ (EqualType _ _) = lift $ declFail "equality not allowed in instance";

    constructInfoExpr :: Name -> [(Name,Name)] -> SimpleType -> M Exp;
    constructInfoExpr knowledgeN varMap (AppType tp1 tp2) = do
    {
        exp1 <- constructInfoExpr knowledgeN varMap tp1;
        exp2 <- constructInfoExpr knowledgeN varMap tp2;
        lift $ [e|R.applyInfo $(return exp1) $(return exp2)|];
    };
    constructInfoExpr _knowledgeN varMap (VarType typeN) = case lookup typeN varMap of
    {
        Just subjectN -> return $ VarE subjectN;
        Nothing -> lift $ declFail $ "type variable not found: " ++ show typeN;
    };
    constructInfoExpr _knowledgeN _varMap (ConsType tp) = lift [e|info :: R.Info $(return $ tp)|];
    constructInfoExpr knowledgeN varMap (FamilyType name vc) = do
    {
        let
        {
            (typeN,constructorN) = vfNames name;

            typeFamilyInfoExpr :: Exp -> [SimpleType] -> M Exp;
            typeFamilyInfoExpr expr [] = return expr;
            typeFamilyInfoExpr expr (t:tt) = do
            {
                texp <- constructInfoExpr knowledgeN varMap t;
                exptexp <- lift $ [e|R.applyInfo $(return expr) $(return texp)|];
                typeFamilyInfoExpr exptexp tt;
            };
        };
        tfTypeExpr <- lift [e|info :: R.Info $(return $ ConT typeN)|];
        infoExpr <- typeFamilyInfoExpr tfTypeExpr vc;
        resultN <- lift $ newName "_var";
        writeQ $ bindS [p|R.ValueFact $(conP constructorN [varP resultN])|] [e|ask $(varE knowledgeN) $(return infoExpr)|];
        return $ VarE resultN;
    };
    constructInfoExpr _knowledgeN _varMap (EqualType _ _) = lift $ declFail "equality not allowed in context expression";

    matchContext :: Name -> [(Name,Name)] -> Type -> M ();
    matchContext knowledgeN varMap tp = do
    {
        st <- lift $ typeToSimple tp;
        case st of
        {
            EqualType st1 st2 -> do
            {
                info1 <- constructInfoExpr knowledgeN varMap st1;
                info2 <- constructInfoExpr knowledgeN varMap st2;
                writeQ $ bindS [p|ReflH|] [e|testHetEquality $(return info1) $(return info2)|];
            };
            _ -> do
            {
                infoExpr <- constructInfoExpr knowledgeN varMap st;
                writeQ $ bindS [p|R.ConstraintFact|] [e|ask $(varE knowledgeN) $(return infoExpr)|];
            };
        }
    };

    instanceStmts :: [Type] -> Type -> Name -> Name -> M ();
    instanceStmts ctxt tp knowledgeN subjectN = do
    {
        st <- lift $ typeToSimple tp;
        varMap <- deconstruct subjectN st;
        traverse_ (matchContext knowledgeN varMap) ctxt;
        matchStmtE <- lift [e|return R.ConstraintFact|];
        write $ NoBindS matchStmtE;
    };

    assocTypeStmts :: Dec -> Name -> Name -> M ();
    assocTypeStmts (TySynInstD patName (TySynEqn patArgsTs exprT)) knowledgeN subjectN = do
    {
        patArgsSTs <- lift $ traverse typeToSimple patArgsTs;
        varMap <- deconstruct subjectN $ FamilyType patName patArgsSTs;
        exprST <- lift $ typeToSimple exprT;
        infoExpr <- constructInfoExpr knowledgeN varMap exprST;
        let
        {
            (_,constructorN) = vfNames patName;
        };
        writeQ $ noBindS $ [e|return $ R.ValueFact ($(conE constructorN) $(return infoExpr))|];
    };
    assocTypeStmts _ _ _ = return ();

    stmtsE :: (Name -> Name -> M ()) -> Q Exp;
    stmtsE getM = do
    {
        knowledgeN <- newName "_knowledge";
        subjectN <- newName "_subject";
        stmts <- execWriterT $ getM knowledgeN subjectN;
        [e|MkKnowledge $ \ $(varP knowledgeN) $(varP subjectN) -> $(return $ DoE stmts)|];
    };

    instanceIE :: [Type] -> Type -> [Dec] -> Q [Exp];
    instanceIE ctxt tp decls = do
    {
        instE <- stmtsE $ instanceStmts ctxt tp;
        assocTypeEs <- traverse (stmtsE . assocTypeStmts) decls;
        return $ instE : assocTypeEs;
    };

    declIE :: Dec -> Q [Exp];
    declIE (InstanceD _ ctxt tp decls) = instanceIE ctxt tp decls;
    declIE decl = declFail $ "non-instance declaration: " ++ show decl;

    declInfo :: Q [Dec] -> Q Exp;
    declInfo qds = do
    {
        ds <- qds;
        exprs <- traverse declIE ds;
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

    instInfo :: Q Type -> Q Exp;
    instInfo qtp = do
    {
        tp <- qtp;
        (name,args) <- headForm tp;
        declInfo $ reifyInstances name args;
    };
}
