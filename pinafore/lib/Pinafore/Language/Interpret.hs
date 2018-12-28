module Pinafore.Language.Interpret
    ( interpretTopExpression
    , interpretTopDeclarations
    ) where

import Data.Graph
import Pinafore.Base
import Pinafore.Language.EntityType
import Pinafore.Language.Expression
import Pinafore.Language.Interpret.Type
import Pinafore.Language.Morphism
import Pinafore.Language.Name
import Pinafore.Language.NamedEntity
import Pinafore.Language.Read.RefNotation
import Pinafore.Language.Syntax
import Pinafore.Language.Type
import Shapes

interpretPattern :: SyntaxPattern -> Name
interpretPattern (MkSyntaxPattern n) = n

interpretExpression ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => SyntaxExpression baseedit
    -> RefExpression baseedit
interpretExpression (MkSyntaxExpression spos sexpr) = interpretExpression' spos sexpr

getBindingNode :: SyntaxBinding baseedit -> (SyntaxBinding baseedit, Name, [Name])
getBindingNode b@(MkSyntaxBinding _ _ n _ _) = (b, n, setToList $ syntaxFreeVariables b)

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

interpretExpression' ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => SourcePos
    -> SyntaxExpression' baseedit
    -> RefExpression baseedit
interpretExpression' spos (SEAbstract sargs sbody) = do
    val <- interpretExpression sbody
    let args = fmap interpretPattern sargs
    liftRefNotation $ runSourcePos spos $ qAbstractsExpr args val
interpretExpression' spos (SELet decls sbody) = do
    MkTransform bmap <- liftRefNotation $ interpretDeclarations spos decls
    bmap $ interpretExpression sbody
interpretExpression' _ (SECase _ _) = fail "NYI: case"
interpretExpression' spos (SEApply sf sargs) = do
    f <- interpretExpression sf
    args <- for sargs interpretExpression
    liftRefNotation $ runSourcePos spos $ qApplyAllExpr f args
interpretExpression' _ (SEConst val) = return $ qConstExprAny val
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
                               mkTypeF $
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
interpretBinding (MkSyntaxBinding spos mtype name sargs sexpr) = do
    val <- interpretExpression sexpr
    expr <-
        liftRefNotation $
        runSourcePos spos $ do
            expr <- qAbstractsExpr (fmap interpretPattern sargs) val
            interpretTypeSignature mtype expr
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
