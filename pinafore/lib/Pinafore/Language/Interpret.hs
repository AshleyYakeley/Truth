module Pinafore.Language.Interpret
    ( interpretTopExpression
    , interpretTopDeclarations
    ) where

import Data.Graph
import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.Expression
import Pinafore.Language.If
import Pinafore.Language.Interpret.Type
import Pinafore.Language.Interpret.TypeDecl
import Pinafore.Language.Name
import Pinafore.Language.Read.RefNotation
import Pinafore.Language.Scope
import Pinafore.Language.SpecialForm
import Pinafore.Language.Syntax
import Pinafore.Language.Type
import Pinafore.Language.Var
import Shapes

interpretPatternConstructor :: SyntaxConstructor -> PinaforeSourceScoped (QPatternConstructor)
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

interpretPattern :: SyntaxPattern -> RefNotation QPattern
interpretPattern (MkWithSourcePos _ AnySyntaxPattern) = return qAnyPattern
interpretPattern (MkWithSourcePos _ (VarSyntaxPattern n)) = return $ qVarPattern n
interpretPattern (MkWithSourcePos spos (BothSyntaxPattern spat1 spat2)) = do
    pat1 <- interpretPattern spat1
    pat2 <- interpretPattern spat2
    liftRefNotation $ runSourcePos spos $ qBothPattern pat1 pat2
interpretPattern (MkWithSourcePos spos (ConstructorSyntaxPattern scons spats)) = do
    pc <- liftRefNotation $ runSourcePos spos $ interpretPatternConstructor scons
    pats <- for spats interpretPattern
    liftRefNotation $ runSourcePos spos $ qConstructPattern pc pats
interpretPattern (MkWithSourcePos spos (TypedSyntaxPattern spat stype)) = do
    pat <- interpretPattern spat
    liftRefNotation $
        runSourcePos spos $ do
            mtp <- interpretType @'Positive stype
            case mtp of
                MkAnyW tp -> do
                    MkGreatestDynamicSupertype dtp _ convm <- getGreatestDynamicSupertype tp
                    let
                        pc :: QPatternConstructor
                        pc =
                            toPatternConstructor dtp (ConsListType (mkShimWit tp) NilListType) $ \dt ->
                                fmap (\a -> (a, ())) (shimToFunction convm dt)
                    qConstructPattern pc [pat]

interpretPatternOrName :: SyntaxPattern -> Either Name (RefNotation QPattern)
interpretPatternOrName (MkWithSourcePos _ (VarSyntaxPattern n)) = Left n
interpretPatternOrName pat = Right $ interpretPattern pat

interpretExpression :: SyntaxExpression -> RefExpression
interpretExpression (MkWithSourcePos spos sexpr) = interpretExpression' spos sexpr

getBindingNode :: SyntaxBinding -> (SyntaxBinding, Name, [Name])
getBindingNode b@(MkSyntaxBinding _ _ n _) = (b, n, setToList $ syntaxFreeVariables b)

-- | Group bindings into a topologically-sorted list of strongly-connected components
clumpBindings :: [SyntaxBinding] -> [[SyntaxBinding]]
clumpBindings bb = fmap flattenSCC $ stronglyConnComp $ fmap getBindingNode bb

interpretLetBindingsClump :: SourcePos -> [SyntaxBinding] -> RefNotation a -> RefNotation a
interpretLetBindingsClump spos sbinds ra = do
    bl <- interpretBindings sbinds
    remonadRefNotation
        (MkWMFunction $ \se -> do
             bmap <- runSourcePos spos $ qUncheckedBindingsComponentLetExpr bl
             withNewLetBindings bmap se) $
        ra

interpretLetBindingss :: SourcePos -> [[SyntaxBinding]] -> RefNotation a -> RefNotation a
interpretLetBindingss _ [] ra = ra
interpretLetBindingss spos (b:bb) ra = interpretLetBindingsClump spos b $ interpretLetBindingss spos bb ra

interpretLetBindings :: SourcePos -> [SyntaxBinding] -> RefNotation a -> RefNotation a
interpretLetBindings spos sbinds ra = do
    liftRefNotation $ runSourcePos spos $ checkSyntaxBindingsDuplicates sbinds
    interpretLetBindingss spos (clumpBindings sbinds) ra

interpretDeclarations :: [SyntaxDeclaration] -> PinaforeSourceScoped (WMFunction RefNotation RefNotation)
interpretDeclarations decls = do
    let
        typeDecls =
            mapMaybe
                (\case
                     TypeSyntaxDeclaration spos name defn -> Just (spos, name, defn)
                     _ -> Nothing)
                decls
        trs =
            mapMaybe
                (\case
                     SubtypeDeclaration spos sta stb ->
                         Just $ MkWMFunction $ mapSourcePos spos $ interpretSubtypeRelation sta stb
                     _ -> Nothing)
                decls
        sbinds =
            (mapMaybe $ \case
                 BindingSyntaxDeclaration sbind -> Just sbind
                 _ -> Nothing)
                decls
        importmods :: [(SourcePos, ModuleName)]
        importmods =
            (mapMaybe $ \case
                 ImportSyntaxDeclarataion spos mname -> Just (spos, mname)
                 _ -> Nothing)
                decls
    imports <- liftSourcePos $ for importmods $ \(spos, mname) -> runSourcePos spos $ importModule mname
    td <- interpretTypeDeclarations typeDecls
    spos <- askSourcePos
    return $ (MkWMFunction $ remonadRefNotation (compAll imports . td . compAll trs) . interpretLetBindings spos sbinds)

interpretNamedConstructor :: SourcePos -> Name -> RefExpression
interpretNamedConstructor spos n = do
    me <- liftRefNotation $ runSourcePos spos $ lookupLetBinding n
    case me of
        Just e -> return e
        Nothing -> throw $ MkErrorMessage spos $ InterpretConstructorUnknownError n

interpretConstructor :: SourcePos -> SyntaxConstructor -> RefExpression
interpretConstructor _ (SLNumber n) =
    return $
    case numberCheckSafeRational n of
        Just r ->
            case safeRationalCheckInteger r of
                Just i -> qConstExprAny $ jmToValue i
                Nothing -> qConstExprAny $ jmToValue r
        Nothing -> qConstExprAny $ jmToValue n
interpretConstructor _ (SLString v) = return $ qConstExprAny $ jmToValue v
interpretConstructor spos (SLNamedConstructor v) = interpretNamedConstructor spos v
interpretConstructor _ SLPair = return $ qConstExprAny $ jmToValue ((,) :: A -> B -> (A, B))
interpretConstructor _ SLUnit = return $ qConstExprAny $ jmToValue ()

specialFormArg :: PinaforeAnnotation t -> SyntaxAnnotation -> ComposeM Maybe PinaforeSourceScoped t
specialFormArg AnnotAnchor (SAAnchor anchor) = return anchor
specialFormArg AnnotMonoEntityType (SAType st) = liftOuter $ interpretMonoEntityType st
specialFormArg AnnotOpenEntityType (SAType st) = liftOuter $ interpretOpenEntityType st
specialFormArg AnnotConcreteDynamicEntityType (SAType st) = liftOuter $ interpretConcreteDynamicEntityType st
specialFormArg AnnotPositiveType (SAType st) = liftOuter $ interpretType @'Positive st
specialFormArg AnnotNegativeType (SAType st) = liftOuter $ interpretType @'Negative st
specialFormArg _ _ = liftInner Nothing

specialFormArgs ::
       ListType PinaforeAnnotation lt -> [SyntaxAnnotation] -> ComposeM Maybe PinaforeSourceScoped (HList lt)
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
showAnnotation AnnotMonoEntityType = "type"
showAnnotation AnnotOpenEntityType = "type"
showAnnotation AnnotConcreteDynamicEntityType = "type"
showAnnotation AnnotPositiveType = "type"
showAnnotation AnnotNegativeType = "type"

interpretSpecialForm :: Name -> NonEmpty SyntaxAnnotation -> PinaforeSourceScoped QValue
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
interpretCase (MkSyntaxCase spat sexpr) = do
    pat <- interpretPattern spat
    expr <- interpretExpressionShadowed (sealedPatternNames pat) sexpr
    return (pat, expr)

interpretExpressionShadowed :: [Name] -> SyntaxExpression -> RefExpression
interpretExpressionShadowed names sbody =
    remonadRefNotation (MkWMFunction $ withRemovedBindings names) $ interpretExpression sbody

interpretExpression' :: SourcePos -> SyntaxExpression' -> RefExpression
interpretExpression' spos (SEAbstract spat sbody) =
    case interpretPatternOrName spat of
        Left name -> do
            val <- interpretExpressionShadowed [name] sbody
            liftRefNotation $ runSourcePos spos $ qAbstractExpr name val
        Right mpat -> do
            pat <- mpat
            val <- interpretExpressionShadowed (sealedPatternNames pat) sbody
            liftRefNotation $ runSourcePos spos $ qCaseAbstract [(pat, val)]
interpretExpression' spos (SELet decls sbody) = do
    MkWMFunction bmap <- liftRefNotation $ runSourcePos spos $ interpretDeclarations decls
    bmap $ interpretExpression sbody
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

interpretTypeSignature :: Maybe SyntaxType -> PinaforeExpression -> PinaforeSourceScoped PinaforeExpression
interpretTypeSignature Nothing expr = return expr
interpretTypeSignature (Just st) expr = do
    at <- interpretType st
    qSubsumeExpr (mapAnyW mkShimWit at) expr

interpretBinding :: SyntaxBinding -> RefNotation QBindings
interpretBinding (MkSyntaxBinding spos mtype name sexpr) = do
    rexpr <- interpretExpression sexpr
    expr <- liftRefNotation $ runSourcePos spos $ interpretTypeSignature mtype rexpr
    return $ qBindExpr name expr

interpretBindings :: [SyntaxBinding] -> RefNotation QBindings
interpretBindings sbinds = do
    qbinds <- for sbinds interpretBinding
    return $ mconcat qbinds

interpretTopDeclarations :: SyntaxTopDeclarations -> PinaforeScoped (WMFunction PinaforeScoped PinaforeScoped)
interpretTopDeclarations (MkSyntaxTopDeclarations spos sdecls) = do
    MkWMFunction f <- runSourcePos spos $ interpretDeclarations sdecls
    return $ MkWMFunction $ \a -> runRefNotation spos $ f $ liftRefNotation a

interpretTopExpression :: SyntaxExpression -> PinaforeScoped QExpr
interpretTopExpression sexpr@(MkWithSourcePos spos _) = runRefNotation spos $ interpretExpression sexpr
