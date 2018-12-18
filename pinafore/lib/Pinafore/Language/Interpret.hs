module Pinafore.Language.Interpret
    ( interpretTopExpression
    , interpretTopDeclarations
    ) where

import Pinafore.Base
import Pinafore.Language.EntityType
import Pinafore.Language.Expression
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

interpretDeclarations ::
       HasPinaforeEntityEdit baseedit
    => SourcePos
    -> SyntaxDeclarations baseedit
    -> PinaforeScoped baseedit (Transform (RefNotation baseedit) (RefNotation baseedit))
interpretDeclarations spos (MkSyntaxDeclarations stypedecls sbinds) = do
    MkTypeDecls td <- stypedecls
    return $
        MkTransform $ \ra ->
            td $ do
                bl <- interpretBindings sbinds
                f <- qBindingsLetExpr bl
                remonadRefNotation
                    (\se -> do
                         bmap <- runSourcePos spos f
                         withNewBindings bmap se) $
                    td ra

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
interpretExpression' _ (SEProperty sta stb anchor) =
    liftRefNotation $ do
        MkAnyW eta <- sta
        MkAnyW etb <- stb
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
interpretExpression' _ (SEEntity st anchor) =
    liftRefNotation $ do
        MkAnyW tp <- st
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

interpretBinding ::
       HasPinaforeEntityEdit baseedit => SyntaxBinding baseedit -> RefNotation baseedit (QBindings baseedit)
interpretBinding (MkSyntaxBinding spos name sargs sexpr) = do
    val <- interpretExpression sexpr
    expr <- liftRefNotation $ runSourcePos spos $ qAbstractsExpr (fmap interpretPattern sargs) val
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
