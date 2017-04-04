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

    declFail :: String -> Q a;
    declFail s = fail $ "declInfo: " ++ s;

    deconstruct :: Name -> Type -> M [(Name,Name)];
    deconstruct subjectN (ParensT tp) = deconstruct subjectN tp;
    deconstruct subjectN (VarT typeN) = return [(typeN,subjectN)];
    deconstruct subjectN (AppT tp1 tp2) = do
    {
        sN1 <- lift $ newName "_f";
        sN2 <- lift $ newName "_a";
        writeQ $ bindS [p|R.MkSplitInfo $(varP sN1) $(varP sN2)|] [e|matchInfo $(varE subjectN)|];
        vars1 <- deconstruct sN1 tp1;
        vars2 <- deconstruct sN2 tp2;
        return $ vars1 ++ vars2;
    };
    -- more cases here
    deconstruct subjectN tp = do
    {
        writeQ $ bindS [p|ReflH|] [e|testHetEquality (info :: R.Info $(return $ tp)) $(varE subjectN)|];
        return [];
    };

    constructInfoExpr :: Name -> [(Name,Name)] -> Type -> M Exp;
    constructInfoExpr knowledgeN varMap (ParensT tp) = constructInfoExpr knowledgeN varMap tp;
    constructInfoExpr knowledgeN varMap (AppT tp1 tp2) = do
    {
        exp1 <- constructInfoExpr knowledgeN varMap tp1;
        exp2 <- constructInfoExpr knowledgeN varMap tp2;
        lift $ [e|R.applyInfo $(return exp1) $(return exp2)|];
    };
    constructInfoExpr _knowledgeN varMap (VarT typeN) = case lookup typeN varMap of
    {
        Just subjectN -> return $ VarE subjectN;
        Nothing -> lift $ declFail $ "type variable not found: " ++ show typeN;
    };
    -- more cases here
    constructInfoExpr _knowledgeN _varMap tp = lift [e|info :: R.Info $(return $ tp)|];

    matchContext :: Name -> [(Name,Name)] -> Type -> M ();
    matchContext knowledgeN varMap consT = do
    {
        infoExpr <- constructInfoExpr knowledgeN varMap consT;
        writeQ $ bindS [p|R.ConstraintFact|] [e|ask $(varE knowledgeN) $(return infoExpr)|];
    };

    instanceStmts :: Name -> Name -> [Type] -> Type -> M ();
    instanceStmts knowledgeN subjectN ctxt tp = do
    {
        varMap <- deconstruct subjectN tp;
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
