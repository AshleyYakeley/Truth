module Data.Reity.Template(declInfo) where
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
    deconstruct _ (FamilyType name _) = lift $ declFail $ "type family not allowed in instance: " ++ show name;
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
            typeName = nameBase name ++ "Info";
            constructorName = "Mk" ++ typeName;

            typeN = mkName typeName;
            constructorN = mkName constructorName;

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

    instanceStmts :: Name -> Name -> [Type] -> Type -> M ();
    instanceStmts knowledgeN subjectN ctxt tp = do
    {
        st <- lift $ typeToSimple tp;
        varMap <- deconstruct subjectN st;
        traverse_ (matchContext knowledgeN varMap) ctxt;
        matchStmtE <- lift [e|return R.ConstraintFact|];
        write $ NoBindS matchStmtE;
    };

    instanceIE :: [Type] -> Type -> Q Exp;
    instanceIE ctxt tp = do
    {
        knowledgeN <- newName "_knowledge";
        subjectN <- newName "_subject";
        stmts <- execWriterT $ instanceStmts knowledgeN subjectN ctxt tp;
        [e|MkKnowledge $ \ $(varP knowledgeN) $(varP subjectN) -> $(return $ DoE stmts)|];
    };

    declIE :: Dec -> Q Exp;
    declIE (InstanceD _ ctxt tp _) = instanceIE ctxt tp;
    declIE decl = declFail $ "non-instance declaration: " ++ show decl;

    declInfo :: Q [Dec] -> Q Exp;
    declInfo qds = do
    {
        ds <- qds;
        exprs <- traverse declIE ds;
        [e|mconcat $(return $ ListE exprs)|];
    };
}
