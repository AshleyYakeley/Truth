{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Expression
    ( module Pinafore.Language.Expression
    , PinaforeScoped
    ) where

import Language.Expression.Bindings
import Language.Expression.Dolan
import Language.Expression.Error
import Language.Expression.Sealed
import Language.Expression.TypeSystem
import Pinafore.Language.Convert
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Language.Type.Subsume ()
import Shapes

type QExpr baseedit = TSSealedExpression (PinaforeTypeSystem baseedit)

type QPattern baseedit = TSSealedPattern (PinaforeTypeSystem baseedit)

type QPatternConstructor baseedit = TSPatternConstructor (PinaforeTypeSystem baseedit)

type QValue baseedit = TSValue (PinaforeTypeSystem baseedit)

qConstExprAny :: forall baseedit. QValue baseedit -> QExpr baseedit
qConstExprAny = tsConst @(PinaforeTypeSystem baseedit)

qConstExpr ::
       forall baseedit a. ToPinaforeType baseedit a
    => a
    -> QExpr baseedit
qConstExpr a = qConstExprAny $ jmToValue a

qVarExpr :: forall baseedit. Name -> QExpr baseedit
qVarExpr name = tsVar @(PinaforeTypeSystem baseedit) name

qAbstractExpr :: forall baseedit. Name -> QExpr baseedit -> PinaforeSourceScoped baseedit (QExpr baseedit)
qAbstractExpr name expr = tsAbstract @(PinaforeTypeSystem baseedit) name expr

qAbstractsExpr :: [Name] -> QExpr baseedit -> PinaforeSourceScoped baseedit (QExpr baseedit)
qAbstractsExpr [] e = return e
qAbstractsExpr (n:nn) e = do
    e' <- qAbstractsExpr nn e
    qAbstractExpr n e'

qVarPattern :: forall baseedit. Name -> QPattern baseedit
qVarPattern = tsVarPattern @(PinaforeTypeSystem baseedit)

qAnyPattern :: forall baseedit. QPattern baseedit
qAnyPattern = tsAnyPattern @(PinaforeTypeSystem baseedit)

qBothPattern ::
       forall baseedit. QPattern baseedit -> QPattern baseedit -> PinaforeSourceScoped baseedit (QPattern baseedit)
qBothPattern = tsBothPattern @(PinaforeTypeSystem baseedit)

qToPatternConstructor ::
       forall baseedit t lt.
       ( ToListShimWit PinaforeShim (PinaforeType baseedit 'Positive) lt
       , FromShimWit JMShim (PinaforeType baseedit 'Negative) t
       )
    => (t -> Maybe (HList lt))
    -> QPatternConstructor baseedit
qToPatternConstructor = toPatternConstructor (fromJMShimWit @(PinaforeType baseedit 'Negative)) toListShimWit

qApplyPatternConstructor ::
       forall baseedit.
       QPatternConstructor baseedit
    -> QPattern baseedit
    -> PinaforeSourceScoped baseedit (QPatternConstructor baseedit)
qApplyPatternConstructor = tsApplyPatternConstructor @(PinaforeTypeSystem baseedit)

qSealPatternConstructor ::
       forall baseedit m. MonadError ExpressionError m
    => QPatternConstructor baseedit
    -> m (QPattern baseedit)
qSealPatternConstructor = tsSealPatternConstructor @(PinaforeTypeSystem baseedit)

qApplyAllPatternConstructor ::
       forall baseedit.
       QPatternConstructor baseedit
    -> [QPattern baseedit]
    -> PinaforeSourceScoped baseedit (QPatternConstructor baseedit)
qApplyAllPatternConstructor pc [] = return pc
qApplyAllPatternConstructor pc (pat:pats) = do
    pc' <- qApplyPatternConstructor pc pat
    qApplyAllPatternConstructor pc' pats

qConstructPattern ::
       forall baseedit.
       QPatternConstructor baseedit
    -> [QPattern baseedit]
    -> PinaforeSourceScoped baseedit (QPattern baseedit)
qConstructPattern pc pats = do
    pc' <- qApplyAllPatternConstructor pc pats
    qSealPatternConstructor pc'

qCase ::
       forall baseedit.
       QExpr baseedit
    -> [(QPattern baseedit, QExpr baseedit)]
    -> PinaforeSourceScoped baseedit (QExpr baseedit)
qCase = tsCase @(PinaforeTypeSystem baseedit)

qFunctionPosWitness ::
       forall baseedit a b.
       PinaforeShimWit baseedit 'Negative a
    -> PinaforeShimWit baseedit 'Positive b
    -> PinaforeShimWit baseedit 'Positive (a -> b)
qFunctionPosWitness = tsFunctionPosShimWit @(PinaforeTypeSystem baseedit)

qFunctionPosWitnesses ::
       ListType (PinaforeShimWit baseedit 'Negative) a
    -> PinaforeShimWit baseedit 'Positive b
    -> PinaforeShimWit baseedit 'Positive (HList a -> b)
qFunctionPosWitnesses NilListType tb = mapShimWit (toEnhanced $ \ub -> ub ()) tb
qFunctionPosWitnesses (ConsListType ta la) tb =
    mapShimWit (toEnhanced $ \f a l -> f (a, l)) $ qFunctionPosWitness ta $ qFunctionPosWitnesses la tb

qCaseAbstract ::
       forall baseedit. [(QPattern baseedit, QExpr baseedit)] -> PinaforeSourceScoped baseedit (QExpr baseedit)
qCaseAbstract = tsCaseAbstract @(PinaforeTypeSystem baseedit)

qApplyExpr :: forall baseedit. QExpr baseedit -> QExpr baseedit -> PinaforeSourceScoped baseedit (QExpr baseedit)
qApplyExpr exprf expra = tsApply @(PinaforeTypeSystem baseedit) exprf expra

qApplyAllExpr :: QExpr baseedit -> [QExpr baseedit] -> PinaforeSourceScoped baseedit (QExpr baseedit)
qApplyAllExpr e [] = return e
qApplyAllExpr e (a:aa) = do
    e' <- qApplyExpr e a
    qApplyAllExpr e' aa

qEmptyList :: QExpr baseedit
qEmptyList = qConstExpr ([] :: [BottomType])

qConsList :: QExpr baseedit
qConsList = qConstExpr ((:) :: UVar "a" -> [UVar "a"] -> [UVar "a"])

qSequenceExpr :: [QExpr baseedit] -> PinaforeSourceScoped baseedit (QExpr baseedit)
qSequenceExpr [] = return $ qEmptyList
qSequenceExpr (e:ee) = do
    ee' <- qSequenceExpr ee
    qApplyAllExpr qConsList [e, ee']

type QBindings baseedit = TSBindings (PinaforeTypeSystem baseedit)

qBindExpr :: forall baseedit. Name -> QExpr baseedit -> QBindings baseedit
qBindExpr = singleBinding

qBindVal :: ToPinaforeType baseedit t => Name -> t -> QBindings baseedit
qBindVal name val = qBindExpr name $ qConstExpr val

qLetExpr :: forall baseedit. Name -> QExpr baseedit -> QExpr baseedit -> PinaforeSourceScoped baseedit (QExpr baseedit)
qLetExpr name exp body = tsLet @(PinaforeTypeSystem baseedit) name exp body

qUncheckedBindingsComponentLetExpr ::
       forall baseedit. QBindings baseedit -> PinaforeSourceScoped baseedit (StrictMap Name (QExpr baseedit))
qUncheckedBindingsComponentLetExpr = tsUncheckedComponentLet @(PinaforeTypeSystem baseedit)

qValuesLetExpr :: forall baseedit. StrictMap Name (QValue baseedit) -> StrictMap Name (QExpr baseedit)
qValuesLetExpr = tsValuesLet @(PinaforeTypeSystem baseedit)

qEvalExpr ::
       forall baseedit m. MonadError ExpressionError m
    => QExpr baseedit
    -> m (QValue baseedit)
qEvalExpr expr = tsEval @(PinaforeTypeSystem baseedit) expr

typedAnyToPinaforeVal ::
       forall baseedit t. FromPinaforeType baseedit t
    => QValue baseedit
    -> PinaforeSourceScoped baseedit t
typedAnyToPinaforeVal = tsAnyToVal @(PinaforeTypeSystem baseedit) fromJMShimWit

qSubsumeExpr ::
       forall baseedit.
       AnyW (PinaforeShimWit baseedit 'Positive)
    -> PinaforeExpression baseedit
    -> PinaforeSourceScoped baseedit (PinaforeExpression baseedit)
qSubsumeExpr t expr = tsSubsume @(PinaforeTypeSystem baseedit) t expr
