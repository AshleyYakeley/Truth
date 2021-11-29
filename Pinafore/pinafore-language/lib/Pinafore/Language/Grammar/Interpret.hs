module Pinafore.Language.Grammar.Interpret
    ( interpretTopExpression
    , interpretModule
    , interpretTopDeclarations
    , interpretType
    ) where

import Data.Graph
import Pinafore.Base
import Pinafore.Language.DefDoc
import Pinafore.Language.DocTree
import Pinafore.Language.Error
import Pinafore.Language.ExprShow
import Pinafore.Language.Expression
import Pinafore.Language.Grammar.Interpret.RefNotation
import Pinafore.Language.Grammar.Interpret.ScopeBuilder
import Pinafore.Language.Grammar.Interpret.Type
import Pinafore.Language.Grammar.Interpret.TypeDecl
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.If
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type
import Pinafore.Language.Var
import Pinafore.Language.VarID
import Pinafore.Markdown
import Shapes

type instance EntryDoc PinaforeTypeSystem = DefDoc

interpretPatternConstructor :: SyntaxConstructor -> PinaforeSourceInterpreter (QPatternConstructor)
interpretPatternConstructor (SLNamedConstructor name) = lookupPatternConstructor name
interpretPatternConstructor (SLNumber v) =
    return $
    qToPatternConstructor $ \v' ->
        if v == v'
            then Just ()
            else Nothing
interpretPatternConstructor (SLString v) =
    return $
    qToPatternConstructor $ \v' ->
        if v == v'
            then Just ()
            else Nothing
interpretPatternConstructor SLUnit = return $ qToPatternConstructor $ \() -> Just ()
interpretPatternConstructor SLPair = return $ qToPatternConstructor $ \(a :: A, b :: B) -> Just $ (a, (b, ()))

interpretPattern :: SyntaxPattern -> ScopeBuilder QPattern
interpretPattern (MkWithSourcePos _ AnySyntaxPattern) = return qAnyPattern
interpretPattern (MkWithSourcePos _ (VarSyntaxPattern n)) = do
    vid <- allocateVarScopeBuilder n
    return $ qVarPattern vid
interpretPattern (MkWithSourcePos spos (BothSyntaxPattern spat1 spat2)) = do
    pat1 <- interpretPattern spat1
    pat2 <- interpretPattern spat2
    lift $ liftRefNotation $ runSourcePos spos $ qBothPattern pat1 pat2
interpretPattern (MkWithSourcePos spos (ConstructorSyntaxPattern scons spats)) = do
    pc <- lift $ liftRefNotation $ runSourcePos spos $ interpretPatternConstructor scons
    pats <- for spats interpretPattern
    lift $ liftRefNotation $ runSourcePos spos $ qConstructPattern pc pats
interpretPattern (MkWithSourcePos spos (TypedSyntaxPattern spat stype)) = do
    pat <- interpretPattern spat
    lift $
        liftRefNotation $
        runSourcePos spos $ do
            mtp <- interpretType @'Positive stype
            case mtp of
                MkAnyW tp -> do
                    dtp <- getGreatestDynamicSupertype tp
                    let
                        pc :: QPatternConstructor
                        pc =
                            toPatternConstructor dtp (ConsListType (mkPolarShimWit tp) NilListType) $
                            fmap $ \a -> (a, ())
                    qConstructPattern pc [pat]

interpretPatternOrName :: SyntaxPattern -> Either Name (ScopeBuilder QPattern)
interpretPatternOrName (MkWithSourcePos _ (VarSyntaxPattern n)) = Left n
interpretPatternOrName pat = Right $ interpretPattern pat

interpretExpression :: SyntaxExpression -> RefExpression
interpretExpression (MkWithSourcePos spos sexpr) = interpretExpression' spos sexpr

type DocSyntaxBinding = (Markdown, SyntaxBinding)

getBindingName :: DocSyntaxBinding -> Name
getBindingName (_, (MkSyntaxBinding _ _ n _)) = n

getBindingNode :: DocSyntaxBinding -> (DocSyntaxBinding, Name, [Name])
getBindingNode db@(_, b@(MkSyntaxBinding _ _ n _)) = (db, n, setToList $ syntaxFreeVariables b)

-- | Group bindings into a topologically-sorted list of strongly-connected components
clumpBindings :: [DocSyntaxBinding] -> [[DocSyntaxBinding]]
clumpBindings bb = fmap flattenSCC $ stronglyConnComp $ fmap getBindingNode bb

composeMap :: Ord b => Map b c -> Map a b -> Map a c
composeMap m = mapMaybe $ \k -> lookup k m

interpretRecursiveLetBindingsClump :: SourcePos -> [DocSyntaxBinding] -> ScopeBuilder ()
interpretRecursiveLetBindingsClump spos sbinds = do
    items <-
        for sbinds $ \sbind -> do
            let name = getBindingName sbind
            vid <- allocateVarScopeBuilder name
            return (name, vid, sbind)
    let nvmap = mapFromList $ fmap (\(name, vid, _) -> (name, vid)) items
    bl <- lift $ interpretBindings $ fmap (\(_, vid, sbind) -> (sbind, vid)) items
    interpScopeBuilder $ \se -> do
        bmap <- runSourcePos spos $ qUncheckedBindingsRecursiveLetExpr bl
        withNewLetBindings (composeMap bmap nvmap) se

interpretRecursiveLetBindingss :: SourcePos -> [[DocSyntaxBinding]] -> ScopeBuilder ()
interpretRecursiveLetBindingss spos bb = for_ bb $ interpretRecursiveLetBindingsClump spos

interpretRecursiveLetBindings :: SourcePos -> [DocSyntaxBinding] -> ScopeBuilder ()
interpretRecursiveLetBindings spos sbinds = do
    lift $ liftRefNotation $ runSourcePos spos $ checkSyntaxBindingsDuplicates $ fmap snd sbinds
    interpretRecursiveLetBindingss spos $ clumpBindings sbinds

interpretSequentialLetBinding :: DocSyntaxBinding -> ScopeBuilder ()
interpretSequentialLetBinding sbind = do
    let n = getBindingName sbind
    vid <- allocateVarScopeBuilder n
    b <- lift $ interpretBinding (sbind, vid)
    interpScopeBuilder $ \se -> do
        bmap <- runSourcePos (getSourcePos $ snd sbind) $ qBindingSequentialLetExpr b
        withNewLetBindings (composeMap bmap $ singletonMap n vid) se

interpretRecursiveDocDeclarations :: SourcePos -> [SyntaxWithDoc SyntaxDirectDeclaration] -> ScopeBuilder Docs
interpretRecursiveDocDeclarations dspos ddecls = do
    let
        interp (MkSyntaxWithDoc doc decl) =
            case decl of
                TypeSyntaxDeclaration spos name defn -> let
                    docName = toText name
                    docValueType = ""
                    docType = TypeDocType
                    docDescription = doc
                    in (pure (spos, name, doc, defn), mempty, mempty, pure $ EntryDocTreeEntry $ MkDefDoc {..})
                SubtypeSyntaxDeclaration spos sta stb ->
                    (mempty, interpretSubtypeRelation spos doc sta stb, mempty, mempty)
                BindingSyntaxDeclaration sbind@(MkSyntaxBinding _ mtype name _) -> let
                    docName = toText name
                    docValueType =
                        case mtype of
                            Nothing -> ""
                            Just st -> exprShow st
                    docType = ValueDocType
                    docDescription = doc
                    in (mempty, mempty, pure (doc, sbind), pure $ EntryDocTreeEntry $ MkDefDoc {..})
        (typeDecls, subtypeSB, bindingDecls, docDecls) = mconcat $ fmap interp ddecls
    interpScopeBuilder $ interpretRecursiveTypeDeclarations typeDecls
    stDocDecls <- subtypeSB
    interpretRecursiveLetBindings dspos bindingDecls
    return $ stDocDecls <> docDecls

interpretExpose :: SyntaxExpose -> RefNotation (Docs -> Docs, PinaforeScope)
interpretExpose (SExpExpose spos names) = do
    scope <- liftRefNotation $ runSourcePos spos $ exportScope names
    return (exposeDocs names, scope)
interpretExpose (SExpLet sdecls expose) =
    runScopeBuilder (interpretDocDeclarations sdecls) $ \olddoc -> do
        (restrict, scope) <- interpretExpose expose
        return (\newdoc -> restrict $ olddoc <> newdoc, scope)

interpretExposeDeclaration :: SyntaxExpose -> ScopeBuilder Docs
interpretExposeDeclaration sexp = do
    (docs, scope) <- lift $ interpretExpose sexp
    pureScopeBuilder scope
    return $ docs []

interpretImportDeclaration :: SourcePos -> ModuleName -> ScopeBuilder Docs
interpretImportDeclaration spos modname = do
    newmod <- lift $ liftRefNotation $ runSourcePos spos $ getModule modname
    pureScopeBuilder (moduleScope newmod)
    return [TreeDocTreeEntry $ moduleDoc newmod]

interpretDocDeclaration :: SyntaxWithDoc SyntaxDeclaration -> ScopeBuilder Docs
interpretDocDeclaration (MkSyntaxWithDoc doc decl) =
    case decl of
        ExposeSyntaxDeclaration _ sexp -> interpretExposeDeclaration sexp
        ImportSyntaxDeclaration spos modname Nothing -> interpretImportDeclaration spos modname
        ImportSyntaxDeclaration spos modname (Just names) ->
            refScopeBuilder $
            runScopeBuilder (interpretImportDeclaration spos modname) $ \docentries -> do
                scope <- liftRefNotation $ runSourcePos spos $ exportScope names
                return $ do
                    pureScopeBuilder scope
                    return (exposeDocs names docentries)
        DirectSyntaxDeclaration (TypeSyntaxDeclaration spos name defn) -> do
            interpScopeBuilder (interpretTypeDeclaration spos name doc defn)
            let
                docName = toText name
                docValueType = ""
                docType = TypeDocType
                docDescription = doc
            return $ defDocs MkDefDoc {..}
        DirectSyntaxDeclaration (SubtypeSyntaxDeclaration spos sta stb) -> interpretSubtypeRelation spos doc sta stb
        DirectSyntaxDeclaration (BindingSyntaxDeclaration sbind@(MkSyntaxBinding _ mtype name _)) -> do
            interpretSequentialLetBinding (doc, sbind)
            let
                docName = toText name
                docValueType =
                    case mtype of
                        Nothing -> ""
                        Just st -> exprShow st
                docType = ValueDocType
                docDescription = doc
            return $ defDocs MkDefDoc {..}
        RecursiveSyntaxDeclaration spos rdecls -> interpretRecursiveDocDeclarations spos rdecls

interpretDocDeclarations :: [SyntaxWithDoc SyntaxDeclaration] -> ScopeBuilder Docs
interpretDocDeclarations decls = mconcat $ fmap interpretDocDeclaration decls

interpretDeclarations :: [SyntaxWithDoc SyntaxDeclaration] -> RefNotation --> RefNotation
interpretDeclarations decls ma = runScopeBuilder (interpretDocDeclarations decls) $ \_ -> ma

interpretNamedConstructor :: SourcePos -> ReferenceName -> RefExpression
interpretNamedConstructor spos n = do
    me <- liftRefNotation $ runSourcePos spos $ lookupLetBinding n
    case me of
        Just (Right e) -> return e
        _ -> throwErrorType spos $ InterpretConstructorUnknownError n

interpretConstructor :: SourcePos -> SyntaxConstructor -> RefExpression
interpretConstructor _ (SLNumber n) =
    return $
    case decode safeRationalNumber n of
        Just r ->
            case decode integerSafeRational r of
                Just i -> qConstExprAny $ jmToValue i
                Nothing -> qConstExprAny $ jmToValue r
        Nothing -> qConstExprAny $ jmToValue n
interpretConstructor _ (SLString v) = return $ qConstExprAny $ jmToValue v
interpretConstructor spos (SLNamedConstructor v) = interpretNamedConstructor spos v
interpretConstructor _ SLPair = return $ qConstExprAny $ jmToValue ((,) :: A -> B -> (A, B))
interpretConstructor _ SLUnit = return $ qConstExprAny $ jmToValue ()

specialFormArg :: PinaforeAnnotation t -> SyntaxAnnotation -> ComposeM Maybe PinaforeSourceInterpreter t
specialFormArg AnnotAnchor (SAAnchor anchor) = return anchor
specialFormArg AnnotPositiveType (SAType st) = liftOuter $ interpretType @'Positive st
specialFormArg AnnotNegativeType (SAType st) = liftOuter $ interpretType @'Negative st
specialFormArg _ _ = liftInner Nothing

specialFormArgs ::
       ListType PinaforeAnnotation lt -> [SyntaxAnnotation] -> ComposeM Maybe PinaforeSourceInterpreter (HList lt)
specialFormArgs NilListType [] = return ()
specialFormArgs (ConsListType t tt) (a:aa) = do
    v <- specialFormArg t a
    vv <- specialFormArgs tt aa
    return (v, vv)
specialFormArgs _ _ = liftInner Nothing

showSA :: SyntaxAnnotation -> Text
showSA (SAType _) = "type"
showSA (SAAnchor _) = "anchor"

showAnnotation :: PinaforeAnnotation a -> Text
showAnnotation AnnotAnchor = "anchor"
showAnnotation AnnotPositiveType = "type"
showAnnotation AnnotNegativeType = "type"

interpretSpecialForm :: ReferenceName -> NonEmpty SyntaxAnnotation -> PinaforeSourceInterpreter QValue
interpretSpecialForm name annotations = do
    MkSpecialForm largs val <- lookupSpecialForm name
    margs <- getComposeM $ specialFormArgs largs $ toList annotations
    case margs of
        Just args -> val args
        Nothing ->
            throw $
            SpecialFormWrongAnnotationsError
                name
                (listTypeToList showAnnotation largs)
                (fmap showSA $ toList annotations)

interpretConstant :: SourcePos -> SyntaxConstant -> RefExpression
interpretConstant _ SCIfThenElse = return $ qConstExprAny $ jmToValue qifthenelse
interpretConstant _ SCBind = return $ qConstExprAny $ jmToValue qbind
interpretConstant _ SCBind_ = return $ qConstExprAny $ jmToValue qbind_
interpretConstant spos (SCConstructor lit) = interpretConstructor spos lit

interpretCase :: SyntaxCase -> RefNotation (QPattern, QExpr)
interpretCase (MkSyntaxCase spat sexpr) =
    runScopeBuilder (interpretPattern spat) $ \pat -> do
        expr <- interpretExpressionShadowed (sealedPatternNames pat) sexpr
        return (pat, expr)

interpretExpressionShadowed :: [a] -> SyntaxExpression -> RefExpression
interpretExpressionShadowed _names sbody =
    interpretExpression sbody {-hoistRefNotation (MkWMFunction $ withRemovedBindings names) $ -}

interpretExpression' :: SourcePos -> SyntaxExpression' -> RefExpression
interpretExpression' spos (SESubsume sexpr stype) = do
    expr <- interpretExpression sexpr
    liftRefNotation $
        runSourcePos spos $ do
            t <- interpretType stype
            qSubsumeExpr t expr
interpretExpression' spos (SEAbstract spat sbody) =
    case interpretPatternOrName spat of
        Left name ->
            runScopeBuilder (allocateVarScopeBuilder name) $ \vid -> do
                val <- interpretExpressionShadowed [name] sbody
                liftRefNotation $ runSourcePos spos $ qAbstractExpr vid val
        Right mpat ->
            runScopeBuilder mpat $ \pat -> do
                val <- interpretExpressionShadowed (sealedPatternNames pat) sbody
                liftRefNotation $ runSourcePos spos $ qCaseAbstract [(pat, val)]
interpretExpression' _ (SELet sdecls sbody) = interpretDeclarations sdecls $ interpretExpression sbody
interpretExpression' spos (SECase sbody scases) = do
    body <- interpretExpression sbody
    pairs <- for scases interpretCase
    liftRefNotation $ runSourcePos spos $ qCase body pairs
interpretExpression' spos (SEApply sf sarg) = do
    f <- interpretExpression sf
    arg <- interpretExpression sarg
    liftRefNotation $ runSourcePos spos $ qApplyExpr f arg
interpretExpression' spos (SEConst c) = interpretConstant spos c
interpretExpression' spos (SEVar name) = varRefExpr spos name
interpretExpression' spos (SESpecialForm name annots) =
    liftRefNotation $
    runSourcePos spos $ do
        val <- interpretSpecialForm name annots
        return $ qConstExprAny val
interpretExpression' spos (SERef sexpr) = refNotationQuote spos $ interpretExpression sexpr
interpretExpression' spos (SEUnref sexpr) = refNotationUnquote spos $ interpretExpression sexpr
interpretExpression' spos (SEList sexprs) = do
    exprs <- for sexprs interpretExpression
    liftRefNotation $ runSourcePos spos $ qSequenceExpr exprs

checkExprVars :: MonadThrow PinaforeError m => QExpr -> m ()
checkExprVars (MkSealedExpression _ expr) = let
    getBadVarErrors ::
           forall w t. AllWitnessConstraint Show w
        => NameTypeWitness (UnitType VarID) (UnitType' w) t
        -> Maybe ErrorMessage
    getBadVarErrors w@(MkNameWitness (BadVarID spos _) _) =
        Just $ MkErrorMessage spos (ExpressionErrorError $ UndefinedBindingsError [show w]) mempty
    getBadVarErrors _ = Nothing
    errorMessages :: [ErrorMessage]
    errorMessages = catMaybes $ expressionFreeWitnesses getBadVarErrors expr
    in case errorMessages of
           [] -> return ()
           errs -> throw $ MkPinaforeError errs

interpretBinding :: (DocSyntaxBinding, VarID) -> RefNotation QBinding
interpretBinding ((doc, MkSyntaxBinding spos mstype _ sexpr), vid) = do
    mtype <- liftRefNotation $ runSourcePos spos $ for mstype interpretType
    expr <- interpretExpression sexpr
    checkExprVars expr
    return $ qBindExpr vid doc mtype expr

interpretBindings :: [(DocSyntaxBinding, VarID)] -> RefNotation [QBinding]
interpretBindings sbinds = for sbinds interpretBinding

interpretTopDeclarations :: SyntaxTopDeclarations -> PinaforeInterpreter --> PinaforeInterpreter
interpretTopDeclarations (MkSyntaxTopDeclarations spos sdecls) ma =
    runSourcePos spos $ runRefNotation $ interpretDeclarations sdecls $ liftRefNotation ma

interpretTopExpression :: SyntaxExpression -> PinaforeInterpreter QExpr
interpretTopExpression sexpr@(MkWithSourcePos spos _) = runSourcePos spos $ runRefNotation $ interpretExpression sexpr

interpretModule :: ModuleName -> SyntaxModule -> PinaforeSourceInterpreter PinaforeModule
interpretModule moduleName smod = do
    (docs, scope) <- runRefNotation $ interpretExpose smod
    return $ MkModule (MkDocTree (toText moduleName) "" $ docs []) scope
