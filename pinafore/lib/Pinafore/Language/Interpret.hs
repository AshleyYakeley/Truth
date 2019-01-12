module Pinafore.Language.Interpret
    ( interpretTopExpression
    , interpretTopDeclarations
    ) where

import Data.Graph
import Pinafore.Base
import Pinafore.Language.EntityType
import Pinafore.Language.Expression
import Pinafore.Language.If
import Pinafore.Language.Interpret.Type
import Pinafore.Language.Morphism
import Pinafore.Language.Name
import Pinafore.Language.NamedEntity
import Pinafore.Language.Read.RefNotation
import Pinafore.Language.Syntax
import Pinafore.Language.Type
import Shapes

interpretPattern :: SyntaxPattern -> RefNotation baseedit (QPattern baseedit)
interpretPattern (MkSyntaxPattern _ AnySyntaxPattern) = return qAnyPattern
interpretPattern (MkSyntaxPattern _ (VarSyntaxPattern n)) = return $ qVarPattern n
interpretPattern (MkSyntaxPattern spos (BothSyntaxPattern spat1 spat2)) = do
    pat1 <- interpretPattern spat1
    pat2 <- interpretPattern spat2
    liftRefNotation $ runSourcePos spos $ qBothPattern pat1 pat2
interpretPattern (MkSyntaxPattern _ (ConstructorSyntaxPattern _name spats)) = do
    _pats <- for spats interpretPattern
    fail "NYI: constructor patterns"

interpretPatternOrName :: SyntaxPattern -> Either Name (RefNotation baseedit (QPattern baseedit))
interpretPatternOrName (MkSyntaxPattern _ (VarSyntaxPattern n)) = Left n
interpretPatternOrName pat = Right $ interpretPattern pat

interpretExpression ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => SyntaxExpression baseedit
    -> RefExpression baseedit
interpretExpression (MkSyntaxExpression spos sexpr) = interpretExpression' spos sexpr

getBindingNode :: SyntaxBinding baseedit -> (SyntaxBinding baseedit, Name, [Name])
getBindingNode b@(MkSyntaxBinding _ _ n _) = (b, n, setToList $ syntaxFreeVariables b)

-- | Group bindings into a topologically-sorted list of strongly-connected components
clumpBindings :: [SyntaxBinding baseedit] -> [[SyntaxBinding baseedit]]
clumpBindings bb = fmap flattenSCC $ stronglyConnComp $ fmap getBindingNode bb

interpretLetBindingsClump ::
       HasPinaforeEntityEdit baseedit
    => SourcePos
    -> [SyntaxBinding baseedit]
    -> RefNotation baseedit a
    -> RefNotation baseedit a
interpretLetBindingsClump spos sbinds ra = do
    bl <- interpretBindings sbinds
    remonadRefNotation
        (\se -> do
             bmap <- runSourcePos spos $ qUncheckedBindingsComponentLetExpr bl
             withNewBindings bmap se) $
        ra

interpretLetBindingss ::
       HasPinaforeEntityEdit baseedit
    => SourcePos
    -> [[SyntaxBinding baseedit]]
    -> RefNotation baseedit a
    -> RefNotation baseedit a
interpretLetBindingss _ [] ra = ra
interpretLetBindingss spos (b:bb) ra = interpretLetBindingsClump spos b $ interpretLetBindingss spos bb ra

interpretLetBindings ::
       HasPinaforeEntityEdit baseedit
    => SourcePos
    -> [SyntaxBinding baseedit]
    -> RefNotation baseedit a
    -> RefNotation baseedit a
interpretLetBindings spos sbinds ra = do
    checkSyntaxBindingsDuplicates sbinds
    interpretLetBindingss spos (clumpBindings sbinds) ra

interpretDeclarations ::
       HasPinaforeEntityEdit baseedit
    => SourcePos
    -> SyntaxDeclarations baseedit
    -> PinaforeScoped baseedit (Transform (RefNotation baseedit) (RefNotation baseedit))
interpretDeclarations spos (MkSyntaxDeclarations stypedecls sbinds) = do
    MkTypeDecls td <- stypedecls
    return $ MkTransform $ \ra -> td $ interpretLetBindings spos sbinds $ td ra

interpretConstructor :: Name -> RefExpression baseedit
interpretConstructor "True" = return $ qConstExprAny $ toValue True
interpretConstructor "False" = return $ qConstExprAny $ toValue False
interpretConstructor n = fail $ "unknown constructor: " <> show n

interpretLiteral :: SyntaxLiteral -> RefExpression baseedit
interpretLiteral (SLNumber v) = return $ qConstExprAny $ toValue v
interpretLiteral (SLString v) = return $ qConstExprAny $ toValue v
interpretLiteral (SLConstructor v) = interpretConstructor v

interpretConstant :: SyntaxConstant -> RefExpression baseedit
interpretConstant SCIfThenElse = return $ qConstExprAny $ toValue qifthenelse
interpretConstant SCPair = return $ qConstExprAny $ toValue ((,) :: UVar "a" -> UVar "b" -> (UVar "a", UVar "b"))
interpretConstant (SCLiteral lit) = interpretLiteral lit

interpretCase ::
       HasPinaforeEntityEdit baseedit => SyntaxCase baseedit -> RefNotation baseedit (QPattern baseedit, QExpr baseedit)
interpretCase (MkSyntaxCase spat sexpr) = do
    pat <- interpretPattern spat
    expr <- interpretExpression sexpr
    return (pat, expr)

interpretExpression' ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => SourcePos
    -> SyntaxExpression' baseedit
    -> RefExpression baseedit
interpretExpression' spos (SEAbstract spat sbody) = do
    val <- interpretExpression sbody
    case interpretPatternOrName spat of
        Left name -> liftRefNotation $ runSourcePos spos $ qAbstractExpr name val
        Right mpat -> do
            pat <- mpat
            liftRefNotation $ runSourcePos spos $ qCaseAbstract [(pat, val)]
interpretExpression' spos (SELet decls sbody) = do
    MkTransform bmap <- liftRefNotation $ interpretDeclarations spos decls
    bmap $ interpretExpression sbody
interpretExpression' spos (SECase sbody scases) = do
    body <- interpretExpression sbody
    pairs <- for scases interpretCase
    liftRefNotation $ runSourcePos spos $ qCase body pairs
interpretExpression' spos (SEApply sf sarg) = do
    f <- interpretExpression sf
    arg <- interpretExpression sarg
    liftRefNotation $ runSourcePos spos $ qApplyExpr f arg
interpretExpression' _ (SEConst c) = interpretConstant c
interpretExpression' spos (SEVar name) = varRefExpr spos name
interpretExpression' spos (SERef sexpr) = refNotationQuote spos $ interpretExpression sexpr
interpretExpression' _ (SEUnref sexpr) = refNotationUnquote $ interpretExpression sexpr
interpretExpression' spos (SEList sexprs) = do
    exprs <- for sexprs interpretExpression
    liftRefNotation $ runSourcePos spos $ qSequenceExpr exprs
interpretExpression' spos (SEProperty sta stb anchor) =
    liftRefNotation $ do
        MkAnyW eta <- runSourcePos spos $ interpretEntityType sta
        MkAnyW etb <- runSourcePos spos $ interpretEntityType stb
        let
            bta = biTypeF (entityTypeToType eta, entityTypeToType eta)
            btb = biTypeF (entityTypeToType etb, entityTypeToType etb)
            in case (bta, btb, entityTypeEq eta, entityTypeEq etb) of
                   (MkAnyF rta pra, MkAnyF rtb prb, Dict, Dict) ->
                       withSubrepresentative rangeTypeInKind rta $
                       withSubrepresentative rangeTypeInKind rtb $ let
                           typef =
                               singlePinaforeTypeF $
                               mkPTypeF $
                               GroundPinaforeSingularType MorphismPinaforeGroundType $
                               ConsDolanArguments rta $ ConsDolanArguments rtb NilDolanArguments
                           morphism = propertyMorphism (entityAdapter eta) (entityAdapter etb) (MkPredicate anchor)
                           pinamorphism = MkPinaforeMorphism pra prb morphism
                           anyval = toTypeFAnyValue typef pinamorphism
                           in return $ qConstExprAny anyval
interpretExpression' spos (SEEntity st anchor) =
    liftRefNotation $ do
        MkAnyW tp <- runSourcePos spos $ interpretEntityType st
        pt <- makeEntity tp $ MkEntity anchor
        let
            typef = entityTypeToType tp
            anyval = toTypeFAnyValue typef pt
        return $ qConstExprAny anyval

makeEntity :: MonadFail m => EntityType t -> Entity -> m t
makeEntity (SimpleEntityType TopSimpleEntityType) p = return p
makeEntity (SimpleEntityType NewSimpleEntityType) p = return $ MkNewEntity p
makeEntity (SimpleEntityType (NamedSimpleEntityType _)) p = return $ MkNamedEntity p
makeEntity t _ = fail $ "not an open entity type: " <> show t

interpretTypeSignature ::
       Maybe SyntaxType -> PinaforeExpression baseedit -> PinaforeSourceScoped baseedit (PinaforeExpression baseedit)
interpretTypeSignature Nothing expr = return expr
interpretTypeSignature (Just st) expr = do
    at <- interpretType st
    qSubsumeExpr at expr

interpretBinding ::
       HasPinaforeEntityEdit baseedit => SyntaxBinding baseedit -> RefNotation baseedit (QBindings baseedit)
interpretBinding (MkSyntaxBinding spos mtype name sexpr) = do
    val <- interpretExpression sexpr
    expr <- liftRefNotation $ runSourcePos spos $ interpretTypeSignature mtype val
    return $ qBindExpr name expr

interpretBindings ::
       HasPinaforeEntityEdit baseedit => [SyntaxBinding baseedit] -> RefNotation baseedit (QBindings baseedit)
interpretBindings sbinds = do
    qbinds <- for sbinds interpretBinding
    return $ mconcat qbinds

interpretTopDeclarations ::
       HasPinaforeEntityEdit baseedit
    => SyntaxTopDeclarations baseedit
    -> PinaforeScoped baseedit (Transform (PinaforeScoped baseedit) (PinaforeScoped baseedit))
interpretTopDeclarations (MkSyntaxTopDeclarations spos sdecls) = do
    MkTransform f <- interpretDeclarations spos sdecls
    return $ MkTransform $ \a -> runRefNotation $ f $ liftRefNotation a

interpretTopExpression ::
       HasPinaforeEntityEdit baseedit => SyntaxExpression baseedit -> PinaforeScoped baseedit (QExpr baseedit)
interpretTopExpression sexpr = runRefNotation $ interpretExpression sexpr
