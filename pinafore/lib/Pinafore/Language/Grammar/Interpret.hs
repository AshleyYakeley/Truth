module Pinafore.Language.Grammar.Interpret
    ( interpretTopExpression
    , interpretModule
    , interpretTopDeclarations
    , interpretType
    ) where

import Data.Graph
import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.Expression
import Pinafore.Language.Grammar.Interpret.RefNotation
import Pinafore.Language.Grammar.Interpret.Type
import Pinafore.Language.Grammar.Interpret.TypeDecl
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.If
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type
import Pinafore.Language.Var
import Shapes

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

interpretLetBindingsClump :: SourcePos -> [SyntaxBinding] -> MFunction RefNotation RefNotation
interpretLetBindingsClump spos sbinds ra = do
    bl <- interpretBindings sbinds
    remonadRefNotation
        (MkWMFunction $ \se -> do
             bmap <- runSourcePos spos $ qUncheckedBindingsComponentLetExpr bl
             withNewLetBindings bmap se) $
        ra

interpretLetBindingss :: SourcePos -> [[SyntaxBinding]] -> MFunction RefNotation RefNotation
interpretLetBindingss _ [] ra = ra
interpretLetBindingss spos (b:bb) ra = interpretLetBindingsClump spos b $ interpretLetBindingss spos bb ra

interpretLetBindings :: SourcePos -> [SyntaxBinding] -> MFunction RefNotation RefNotation
interpretLetBindings spos sbinds ra = do
    liftRefNotation $ runSourcePos spos $ checkSyntaxBindingsDuplicates sbinds
    interpretLetBindingss spos (clumpBindings sbinds) ra

interpretDeclarations :: SourcePos -> [SyntaxDeclaration] -> MFunction RefNotation RefNotation
interpretDeclarations dspos decls = let
    importDecls :: WMFunction PinaforeInterpreter PinaforeInterpreter
    importDecls =
        compAll $
        (mapMaybe $ \case
             ImportSyntaxDeclarataion spos mname -> Just $ MkWMFunction $ importModule spos mname
             _ -> Nothing)
            decls
    typeDecls :: WMFunction PinaforeInterpreter PinaforeInterpreter
    typeDecls =
        MkWMFunction $
        interpretTypeDeclarations $
        mapMaybe
            (\case
                 TypeSyntaxDeclaration spos name defn -> Just (spos, name, defn)
                 _ -> Nothing)
            decls
    subtypeDecls :: WMFunction PinaforeInterpreter PinaforeInterpreter
    subtypeDecls =
        compAll $
        mapMaybe
            (\case
                 SubtypeDeclaration spos sta stb ->
                     Just $ MkWMFunction $ mapSourcePos spos $ interpretSubtypeRelation sta stb
                 _ -> Nothing)
            decls
    bindingDecls :: MFunction RefNotation RefNotation
    bindingDecls =
        interpretLetBindings dspos $
        (mapMaybe $ \case
             BindingSyntaxDeclaration sbind -> Just sbind
             _ -> Nothing)
            decls
    in remonadRefNotation (importDecls . typeDecls . subtypeDecls) . bindingDecls

interpretNamedConstructor :: SourcePos -> ReferenceName -> RefExpression
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

specialFormArg :: PinaforeAnnotation t -> SyntaxAnnotation -> ComposeM Maybe PinaforeSourceInterpreter t
specialFormArg AnnotAnchor (SAAnchor anchor) = return anchor
specialFormArg AnnotMonoEntityType (SAType st) = liftOuter $ interpretMonoEntityType st
specialFormArg AnnotOpenEntityType (SAType st) = liftOuter $ interpretOpenEntityType st
specialFormArg AnnotConcreteDynamicEntityType (SAType st) = liftOuter $ interpretConcreteDynamicEntityType st
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
showAnnotation AnnotMonoEntityType = "type"
showAnnotation AnnotOpenEntityType = "type"
showAnnotation AnnotConcreteDynamicEntityType = "type"
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
interpretCase (MkSyntaxCase spat sexpr) = do
    pat <- interpretPattern spat
    expr <- interpretExpressionShadowed (sealedPatternNames pat) sexpr
    return (pat, expr)

interpretExpressionShadowed :: [Name] -> SyntaxExpression -> RefExpression
interpretExpressionShadowed names sbody =
    remonadRefNotation (MkWMFunction $ withRemovedBindings names) $ interpretExpression sbody

interpretExpression' :: SourcePos -> SyntaxExpression' -> RefExpression
interpretExpression' spos (SESubsume sexpr stype) = do
    expr <- interpretExpression sexpr
    liftRefNotation $
        runSourcePos spos $ do
            t <- interpretType stype
            qSubsumeExpr t expr
interpretExpression' spos (SEAbstract spat sbody) =
    case interpretPatternOrName spat of
        Left name -> do
            val <- interpretExpressionShadowed [name] sbody
            liftRefNotation $ runSourcePos spos $ qAbstractExpr name val
        Right mpat -> do
            pat <- mpat
            val <- interpretExpressionShadowed (sealedPatternNames pat) sbody
            liftRefNotation $ runSourcePos spos $ qCaseAbstract [(pat, val)]
interpretExpression' spos (SELet sdecls sbody) = interpretDeclarations spos sdecls $ interpretExpression sbody
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

interpretBinding :: SyntaxBinding -> RefNotation QBindings
interpretBinding (MkSyntaxBinding spos mstype name sexpr) = do
    mtype <- liftRefNotation $ runSourcePos spos $ for mstype interpretType
    expr <- interpretExpression sexpr
    return $ qBindExpr name mtype expr

interpretBindings :: [SyntaxBinding] -> RefNotation QBindings
interpretBindings sbinds = do
    qbinds <- for sbinds interpretBinding
    return $ mconcat qbinds

interpretTopDeclarations :: SyntaxTopDeclarations -> MFunction PinaforeInterpreter PinaforeInterpreter
interpretTopDeclarations (MkSyntaxTopDeclarations spos sdecls) ma =
    runRefNotation spos $ interpretDeclarations spos sdecls $ liftRefNotation ma

interpretTopExpression :: SyntaxExpression -> PinaforeInterpreter QExpr
interpretTopExpression sexpr@(MkWithSourcePos spos _) = runRefNotation spos $ interpretExpression sexpr

interpretModule :: SyntaxModule -> PinaforeInterpreter PinaforeScope
interpretModule (MkWithSourcePos spos (SMLet sdecls smod)) =
    runRefNotation spos $ interpretDeclarations spos sdecls $ liftRefNotation $ interpretModule smod
interpretModule (MkWithSourcePos spos (SMExport names)) = runSourcePos spos $ exportScope names
