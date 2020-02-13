module Pinafore.Language.Interpret
    ( interpretTopExpression
    , interpretTopDeclarations
    ) where

import Data.Graph
import Data.Shim
import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.Expression
import Pinafore.Language.If
import Pinafore.Language.Interpret.Type
import Pinafore.Language.Name
import Pinafore.Language.Read.RefNotation
import Pinafore.Language.Syntax
import Pinafore.Language.TypeSystem
import Pinafore.Language.TypeSystem.Show
import Pinafore.Language.Value
import Shapes

type A = UVar "a"

type B = UVar "b"

interpretPatternConstructor :: SyntaxConstructor -> PinaforeSourceScoped baseupdate (QPatternConstructor baseupdate)
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

interpretPattern :: SyntaxPattern -> RefNotation baseupdate (QPattern baseupdate)
interpretPattern (MkSyntaxPattern _ AnySyntaxPattern) = return qAnyPattern
interpretPattern (MkSyntaxPattern _ (VarSyntaxPattern n)) = return $ qVarPattern n
interpretPattern (MkSyntaxPattern spos (BothSyntaxPattern spat1 spat2)) = do
    pat1 <- interpretPattern spat1
    pat2 <- interpretPattern spat2
    liftRefNotation $ runSourcePos spos $ qBothPattern pat1 pat2
interpretPattern (MkSyntaxPattern spos (ConstructorSyntaxPattern scons spats)) = do
    pc <- liftRefNotation $ runSourcePos spos $ interpretPatternConstructor scons
    pats <- for spats interpretPattern
    liftRefNotation $ runSourcePos spos $ qConstructPattern pc pats

interpretPatternOrName :: SyntaxPattern -> Either Name (RefNotation baseupdate (QPattern baseupdate))
interpretPatternOrName (MkSyntaxPattern _ (VarSyntaxPattern n)) = Left n
interpretPatternOrName pat = Right $ interpretPattern pat

interpretExpression :: forall baseupdate. SyntaxExpression baseupdate -> RefExpression baseupdate
interpretExpression (MkSyntaxExpression spos sexpr) = interpretExpression' spos sexpr

getBindingNode :: SyntaxBinding baseupdate -> (SyntaxBinding baseupdate, Name, [Name])
getBindingNode b@(MkSyntaxBinding _ _ n _) = (b, n, setToList $ syntaxFreeVariables b)

-- | Group bindings into a topologically-sorted list of strongly-connected components
clumpBindings :: [SyntaxBinding baseupdate] -> [[SyntaxBinding baseupdate]]
clumpBindings bb = fmap flattenSCC $ stronglyConnComp $ fmap getBindingNode bb

interpretLetBindingsClump ::
       SourcePos -> [SyntaxBinding baseupdate] -> RefNotation baseupdate a -> RefNotation baseupdate a
interpretLetBindingsClump spos sbinds ra = do
    bl <- interpretBindings sbinds
    remonadRefNotation
        (MkWMFunction $ \se -> do
             bmap <- runSourcePos spos $ qUncheckedBindingsComponentLetExpr bl
             withNewBindings bmap se) $
        ra

interpretLetBindingss ::
       SourcePos -> [[SyntaxBinding baseupdate]] -> RefNotation baseupdate a -> RefNotation baseupdate a
interpretLetBindingss _ [] ra = ra
interpretLetBindingss spos (b:bb) ra = interpretLetBindingsClump spos b $ interpretLetBindingss spos bb ra

interpretLetBindings :: SourcePos -> [SyntaxBinding baseupdate] -> RefNotation baseupdate a -> RefNotation baseupdate a
interpretLetBindings spos sbinds ra = do
    liftRefNotation $ runSourcePos spos $ checkSyntaxBindingsDuplicates sbinds
    interpretLetBindingss spos (clumpBindings sbinds) ra

interpretDeclarations ::
       SourcePos
    -> SyntaxDeclarations baseupdate
    -> PinaforeScoped baseupdate (WMFunction (RefNotation baseupdate) (RefNotation baseupdate))
interpretDeclarations spos (MkSyntaxDeclarations stypedecls sbinds) = do
    MkTypeDecls td tr <- stypedecls
    return $ MkWMFunction $ \ra -> td $ tr $ interpretLetBindings spos sbinds ra

interpretNamedConstructor :: SourcePos -> Name -> RefExpression baseupdate
interpretNamedConstructor spos n = do
    me <- liftRefNotation $ runSourcePos spos $ lookupBinding n
    case me of
        Just e -> return e
        Nothing -> throwError $ MkErrorMessage spos $ InterpretConstructorUnknownError n

interpretConstructor :: SourcePos -> SyntaxConstructor -> RefExpression baseupdate
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
interpretConstructor _ SLPair = return $ qConstExprAny $ jmToValue ((,) :: UVar "a" -> UVar "b" -> (UVar "a", UVar "b"))
interpretConstructor _ SLUnit = return $ qConstExprAny $ jmToValue ()

interpretConstant :: SourcePos -> SyntaxConstant -> RefExpression baseupdate
interpretConstant _ SCIfThenElse = return $ qConstExprAny $ jmToValue qifthenelse
interpretConstant _ SCBind = return $ qConstExprAny $ jmToValue qbind
interpretConstant _ SCBind_ = return $ qConstExprAny $ jmToValue qbind_
interpretConstant spos (SCConstructor lit) = interpretConstructor spos lit

interpretCase :: SyntaxCase baseupdate -> RefNotation baseupdate (QPattern baseupdate, QExpr baseupdate)
interpretCase (MkSyntaxCase spat sexpr) = do
    pat <- interpretPattern spat
    expr <- interpretExpression sexpr
    return (pat, expr)

interpretExpression' :: forall baseupdate. SourcePos -> SyntaxExpression' baseupdate -> RefExpression baseupdate
interpretExpression' spos (SEAbstract spat sbody) = do
    val <- interpretExpression sbody
    case interpretPatternOrName spat of
        Left name -> liftRefNotation $ runSourcePos spos $ qAbstractExpr name val
        Right mpat -> do
            pat <- mpat
            liftRefNotation $ runSourcePos spos $ qCaseAbstract [(pat, val)]
interpretExpression' spos (SELet decls sbody) = do
    MkWMFunction bmap <- liftRefNotation $ interpretDeclarations spos decls
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
interpretExpression' spos (SEProperty sta stb anchor) =
    liftRefNotation $ do
        meta <- runSourcePos spos $ interpretEntityType sta
        metb <- runSourcePos spos $ interpretEntityType stb
        case (meta, metb) of
            (MkAnyW eta, MkAnyW etb) -> do
                etan <- runSourcePos spos $ entityToNegativePinaforeType eta
                etbn <- runSourcePos spos $ entityToNegativePinaforeType etb
                let
                    bta = biRangeAnyF (etan, entityToPositivePinaforeType eta)
                    btb = biRangeAnyF (etbn, entityToPositivePinaforeType etb)
                    in case (bta, btb, entityTypeEq eta, entityTypeEq etb) of
                           (MkAnyF rta pra, MkAnyF rtb prb, Dict, Dict) ->
                               withSubrepresentative rangeTypeInKind rta $
                               withSubrepresentative rangeTypeInKind rtb $ let
                                   typef =
                                       singlePinaforeShimWit $
                                       mkPJMShimWit $
                                       GroundPinaforeSingularType MorphismPinaforeGroundType $
                                       ConsDolanArguments rta $ ConsDolanArguments rtb NilDolanArguments
                                   morphism =
                                       propertyMorphism (entityAdapter eta) (entityAdapter etb) (MkPredicate anchor)
                                   pinamorphism = MkPinaforeMorphism pra prb morphism
                                   anyval = MkAnyValue typef pinamorphism
                                   in return $ qConstExprAny anyval
interpretExpression' spos (SEEntity st anchor) =
    liftRefNotation $ do
        mtp <- runSourcePos spos $ interpretEntityType st
        case mtp of
            MkAnyW tp -> do
                pt <- runSourcePos spos $ makeEntity tp $ MkEntity anchor
                let
                    typef = entityToPositivePinaforeType tp
                    anyval = MkAnyValue typef pt
                return $ qConstExprAny anyval

makeEntity :: MonadError ErrorType m => EntityType t -> Entity -> m t
makeEntity (MkEntityType TopEntityGroundType NilArguments) p = return p
makeEntity (MkEntityType NewEntityGroundType NilArguments) p = return $ MkNewEntity p
makeEntity (MkEntityType (OpenEntityGroundType _ _) NilArguments) p = return $ MkOpenEntity p
makeEntity t _ = throwError $ InterpretTypeNotOpenEntityError $ exprShow t

interpretTypeSignature ::
       Maybe SyntaxType
    -> PinaforeExpression baseupdate
    -> PinaforeSourceScoped baseupdate (PinaforeExpression baseupdate)
interpretTypeSignature Nothing expr = return expr
interpretTypeSignature (Just st) expr = do
    at <- interpretType st
    qSubsumeExpr (mapAnyW mkShimWit at) expr

interpretBinding :: SyntaxBinding baseupdate -> RefNotation baseupdate (QBindings baseupdate)
interpretBinding (MkSyntaxBinding spos mtype name sexpr) = do
    rexpr <- interpretExpression sexpr
    expr <- liftRefNotation $ runSourcePos spos $ interpretTypeSignature mtype rexpr
    return $ qBindExpr name expr

interpretBindings :: [SyntaxBinding baseupdate] -> RefNotation baseupdate (QBindings baseupdate)
interpretBindings sbinds = do
    qbinds <- for sbinds interpretBinding
    return $ mconcat qbinds

interpretTopDeclarations ::
       SyntaxTopDeclarations baseupdate
    -> PinaforeScoped baseupdate (WMFunction (PinaforeScoped baseupdate) (PinaforeScoped baseupdate))
interpretTopDeclarations (MkSyntaxTopDeclarations spos sdecls) = do
    MkWMFunction f <- interpretDeclarations spos sdecls
    return $ MkWMFunction $ \a -> runRefNotation spos $ f $ liftRefNotation a

interpretTopExpression :: SyntaxExpression baseupdate -> PinaforeScoped baseupdate (QExpr baseupdate)
interpretTopExpression sexpr@(MkSyntaxExpression spos _) = runRefNotation spos $ interpretExpression sexpr
