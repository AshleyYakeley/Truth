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

    type M = WriterT [Stmt] Q;

    write :: Stmt -> M ();
    write stmt = tell [stmt];

    writeQ :: Q Stmt -> M ();
    writeQ qstmt = do
    {
        stmt <- lift qstmt;
        write stmt;
    };

    data SimpleType = VarType Name | AppType SimpleType SimpleType | ConsType Type | FamilyType Name;

    typeToSimple :: Type -> M SimpleType;
    typeToSimple (ParensT tp) = typeToSimple tp;
    typeToSimple (SigT tp _) = typeToSimple tp;
    typeToSimple (VarT name) = return $ VarType name;
    typeToSimple (AppT t1 t2) = AppType <$> typeToSimple t1 <*> typeToSimple t2;
    typeToSimple (InfixT t1 name t2) = (\st1 st2 -> AppType (AppType (ConsType $ ConT name) st1) st2) <$> typeToSimple t1 <*> typeToSimple t2;
    typeToSimple (ForallT _ _ _) = lift $ declFail "forall";
    typeToSimple WildCardT = lift $ declFail "_ in type";
    typeToSimple tp@(ConT name) = do
    {
        ninfo <- lift $ reify name;
        case ninfo of
        {
            FamilyI _ _ -> return $ FamilyType name;
            _ -> return $ ConsType tp;
        };
    };
    typeToSimple tp = return $ ConsType tp;

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
        writeQ $ bindS [p|ReflH|] [e|testHetEquality (info :: R.Info $(return $ tp)) $(varE subjectN)|];
        return [];
    };
    deconstruct _ (FamilyType name) = lift $ declFail $ "type family not allowed in instance: " ++ show name;

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
    constructInfoExpr _knowledgeN _varMap (FamilyType name) = lift $ declFail $ "NYI: type family not yet allowed in context: " ++ show name;

    matchContext :: Name -> [(Name,Name)] -> Type -> M ();
    matchContext knowledgeN varMap tp = do
    {
        st <- typeToSimple tp;
        infoExpr <- constructInfoExpr knowledgeN varMap st;
        writeQ $ bindS [p|R.ConstraintFact|] [e|ask $(varE knowledgeN) $(return infoExpr)|];
    };

    instanceStmts :: Name -> Name -> [Type] -> Type -> M ();
    instanceStmts knowledgeN subjectN ctxt tp = do
    {
        st <- typeToSimple tp;
        varMap <- deconstruct subjectN st;
        traverse_ (matchContext knowledgeN varMap) ctxt;
        matchStmtE <- lift [e|return R.ConstraintFact|];
        write $ NoBindS matchStmtE;
    };

    instanceIE :: [Type] -> Type -> Q Exp;
    instanceIE ctxt tp = do
    {
        knowledgeN <- newName "_knowledge";
        subjectN <- newName "subject";
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
