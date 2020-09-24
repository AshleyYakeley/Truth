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
import Pinafore.Language.Syntax
import Pinafore.Language.Type
import Pinafore.Language.Value
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
             withNewBindings bmap se) $
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
    td <- interpretTypeDeclarations typeDecls
    spos <- askSourcePos
    return $ MkWMFunction $ remonadRefNotation (td . compAll trs) . interpretLetBindings spos sbinds

interpretNamedConstructor :: SourcePos -> Name -> RefExpression
interpretNamedConstructor spos n = do
    me <- liftRefNotation $ runSourcePos spos $ lookupBinding n
    case me of
        Just e -> return e
        Nothing -> throw $ MkErrorMessage spos $ InterpretConstructorUnknownError n

interpretConstructor :: SourcePos -> SyntaxConstructor -> RefExpression
interpretConstructor _ (SLNumber n) =
    return $
    case checkExactSafeRational n of
        Just r ->
            case safeRationalInteger r of
                Just i -> qConstExprAny $ jmToValue i
                Nothing -> qConstExprAny $ jmToValue r
        Nothing -> qConstExprAny $ jmToValue n
interpretConstructor _ (SLString v) = return $ qConstExprAny $ jmToValue v
interpretConstructor spos (SLNamedConstructor v) = interpretNamedConstructor spos v
interpretConstructor _ SLPair = return $ qConstExprAny $ jmToValue ((,) :: A -> B -> (A, B))
interpretConstructor _ SLUnit = return $ qConstExprAny $ jmToValue ()

interpretSpecialForm :: SourcePos -> SyntaxSpecialForm -> RefExpression
interpretSpecialForm spos (SSFProperty sta stb anchor) =
    liftRefNotation $ do
        meta <- runSourcePos spos $ interpretConcreteEntityType sta
        metb <- runSourcePos spos $ interpretConcreteEntityType stb
        case (meta, metb) of
            (MkAnyW eta, MkAnyW etb) -> do
                etan <- runSourcePos spos $ concreteEntityToNegativePinaforeType eta
                etbn <- runSourcePos spos $ concreteEntityToNegativePinaforeType etb
                let
                    bta = biRangeAnyF (etan, concreteToPositiveDolanType eta)
                    btb = biRangeAnyF (etbn, concreteToPositiveDolanType etb)
                    in case (bta, btb, concreteEntityTypeEq eta, concreteEntityTypeEq etb) of
                           (MkAnyF rta (MkRange praContra praCo), MkAnyF rtb (MkRange prbContra prbCo), Dict, Dict) ->
                               withSubrepresentative rangeTypeInKind rta $
                               withSubrepresentative rangeTypeInKind rtb $ let
                                   typef =
                                       singleDolanShimWit $
                                       mkShimWit $
                                       GroundDolanSingularType MorphismPinaforeGroundType $
                                       ConsDolanArguments rta $ ConsDolanArguments rtb NilDolanArguments
                                   morphism =
                                       propertyMorphism
                                           (concreteEntityAdapter eta)
                                           (concreteEntityAdapter etb)
                                           (MkPredicate anchor)
                                   pinamorphism =
                                       MkLangMorphism $
                                       cfmap3 (MkCatDual $ shimToFunction praContra) $
                                       cfmap2 (shimToFunction praCo) $
                                       cfmap1 (MkCatDual $ shimToFunction prbContra) $
                                       fmap (shimToFunction prbCo) morphism
                                   anyval = MkAnyValue typef pinamorphism
                                   in return $ qConstExprAny anyval
interpretSpecialForm spos (SSFOpenEntity st anchor) =
    liftRefNotation $ do
        mtp <- runSourcePos spos $ interpretConcreteEntityType st
        case mtp of
            MkAnyW tp -> do
                pt <- runSourcePos spos $ makeEntity tp $ MkEntity anchor
                let typef = concreteToPositiveDolanType tp
                return $ qConstExprAny $ MkAnyValue typef pt
interpretSpecialForm spos (SSFNewOpenEntity st) =
    liftRefNotation $ do
        mtp <- runSourcePos spos $ interpretOpenEntityType st
        case mtp of
            MkAnyW (tp :: OpenEntityType tid) -> do
                let
                    pt :: PinaforeAction (OpenEntity tid)
                    pt = liftIO $ newKeyContainerItem @(FiniteSet (OpenEntity tid))
                    typef =
                        actionShimWit $
                        singleDolanShimWit $
                        mkShimWit $
                        GroundDolanSingularType
                            (EntityPinaforeGroundType NilListType $ OpenEntityGroundType tp)
                            NilDolanArguments
                return $ qConstExprAny $ MkAnyValue typef pt
interpretSpecialForm spos (SSFEvaluate st) =
    liftRefNotation $
    runSourcePos spos $ do
        mtp <- interpretType @'Positive st
        spvals <- getSpecialVals
        return $
            case mtp of
                MkAnyW tp -> let
                    eitherShimWit ::
                           forall a b.
                           PinaforeShimWit 'Positive a
                        -> PinaforeShimWit 'Positive b
                        -> PinaforeShimWit 'Positive (Either a b)
                    eitherShimWit swa swb =
                        unPosShimWit swa $ \ta conva ->
                            unPosShimWit swb $ \tb convb ->
                                mapPosShimWit (applyCoPolyShim (cfmap conva) convb) $
                                singleDolanShimWit $
                                mkShimWit $
                                GroundDolanSingularType
                                    (EntityPinaforeGroundType
                                         (ConsListType Refl $ ConsListType Refl NilListType)
                                         EitherEntityGroundType) $
                                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments
                    funcShimWit ::
                           forall a b.
                           PinaforeShimWit 'Negative a
                        -> PinaforeShimWit 'Positive b
                        -> PinaforeShimWit 'Positive (a -> b)
                    funcShimWit swa swb =
                        unNegShimWit swa $ \ta conva ->
                            unPosShimWit swb $ \tb convb ->
                                mapPosShimWit (applyCoPolyShim (ccontramap conva) convb) $
                                singleDolanShimWit $
                                mkShimWit $
                                GroundDolanSingularType FuncPinaforeGroundType $
                                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments
                    textShimWit ::
                           forall polarity. Is PolarityType polarity
                        => PinaforeShimWit polarity Text
                    textShimWit =
                        singleDolanShimWit $
                        mkShimWit $
                        GroundDolanSingularType
                            (EntityPinaforeGroundType NilListType $ LiteralEntityGroundType TextLiteralType)
                            NilDolanArguments
                    valShimWit ::
                           forall t.
                           PinaforeShimWit 'Positive t
                        -> PinaforeShimWit 'Positive (Text -> PinaforeAction (Either Text t))
                    valShimWit t' = funcShimWit textShimWit $ actionShimWit $ eitherShimWit textShimWit t'
                    in qConstExprAny $ MkAnyValue (valShimWit $ mkShimWit tp) $ specialEvaluate spvals tp

interpretConstant :: SourcePos -> SyntaxConstant -> RefExpression
interpretConstant _ SCIfThenElse = return $ qConstExprAny $ jmToValue qifthenelse
interpretConstant _ SCBind = return $ qConstExprAny $ jmToValue qbind
interpretConstant _ SCBind_ = return $ qConstExprAny $ jmToValue qbind_
interpretConstant spos (SCConstructor lit) = interpretConstructor spos lit
interpretConstant spos (SCSpecialForm sf) = interpretSpecialForm spos sf

interpretCase :: SyntaxCase -> RefNotation (QPattern, QExpr)
interpretCase (MkSyntaxCase spat sexpr) = do
    pat <- interpretPattern spat
    expr <- interpretExpression sexpr
    return (pat, expr)

actionShimWit :: forall a. PinaforeShimWit 'Positive a -> PinaforeShimWit 'Positive (PinaforeAction a)
actionShimWit swa =
    unPosShimWit swa $ \ta conva ->
        mapPosShimWit (cfmap conva) $
        singleDolanShimWit $
        mkShimWit $ GroundDolanSingularType ActionPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

interpretExpression' :: SourcePos -> SyntaxExpression' -> RefExpression
interpretExpression' spos (SEAbstract spat sbody) = do
    val <- interpretExpression sbody
    case interpretPatternOrName spat of
        Left name -> liftRefNotation $ runSourcePos spos $ qAbstractExpr name val
        Right mpat -> do
            pat <- mpat
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
interpretExpression' spos (SERef sexpr) = refNotationQuote spos $ interpretExpression sexpr
interpretExpression' spos (SEUnref sexpr) = refNotationUnquote spos $ interpretExpression sexpr
interpretExpression' spos (SEList sexprs) = do
    exprs <- for sexprs interpretExpression
    liftRefNotation $ runSourcePos spos $ qSequenceExpr exprs

makeEntity :: MonadThrow ErrorType m => ConcreteEntityType t -> Entity -> m t
makeEntity (MkConcreteType TopEntityGroundType NilArguments) p = return p
makeEntity (MkConcreteType (OpenEntityGroundType _) NilArguments) p = return $ MkOpenEntity p
makeEntity t _ = throw $ InterpretTypeNotOpenEntityError $ exprShow t

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
