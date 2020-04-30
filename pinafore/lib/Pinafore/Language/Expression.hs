{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Expression
    ( module Pinafore.Language.Expression
    , PinaforeScoped
    ) where

import Data.Shim
import Language.Expression.Bindings
import Language.Expression.Error
import Language.Expression.Sealed
import Language.Expression.TypeSystem
import Pinafore.Language.Convert
import Pinafore.Language.Name
import Pinafore.Language.TypeSystem
import Pinafore.Language.TypeSystem.Subsume ()
import Shapes

type QExpr baseupdate = TSSealedExpression (PinaforeTypeSystem baseupdate)

type QPattern baseupdate = TSSealedPattern (PinaforeTypeSystem baseupdate)

type QPatternConstructor baseupdate = TSPatternConstructor (PinaforeTypeSystem baseupdate)

type QValue baseupdate = TSValue (PinaforeTypeSystem baseupdate)

qConstExprAny :: forall baseupdate. QValue baseupdate -> QExpr baseupdate
qConstExprAny = tsConst @(PinaforeTypeSystem baseupdate)

qConstExpr ::
       forall baseupdate a. ToPinaforeType baseupdate a
    => a
    -> QExpr baseupdate
qConstExpr a = qConstExprAny $ jmToValue a

qVarExpr :: forall baseupdate. Name -> QExpr baseupdate
qVarExpr name = tsVar @(PinaforeTypeSystem baseupdate) name

qAbstractExpr :: forall baseupdate. Name -> QExpr baseupdate -> PinaforeSourceScoped baseupdate (QExpr baseupdate)
qAbstractExpr name expr = tsAbstract @(PinaforeTypeSystem baseupdate) name expr

qAbstractsExpr :: [Name] -> QExpr baseupdate -> PinaforeSourceScoped baseupdate (QExpr baseupdate)
qAbstractsExpr [] e = return e
qAbstractsExpr (n:nn) e = do
    e' <- qAbstractsExpr nn e
    qAbstractExpr n e'

qVarPattern :: forall baseupdate. Name -> QPattern baseupdate
qVarPattern = tsVarPattern @(PinaforeTypeSystem baseupdate)

qAnyPattern :: forall baseupdate. QPattern baseupdate
qAnyPattern = tsAnyPattern @(PinaforeTypeSystem baseupdate)

qBothPattern ::
       forall baseupdate.
       QPattern baseupdate
    -> QPattern baseupdate
    -> PinaforeSourceScoped baseupdate (QPattern baseupdate)
qBothPattern = tsBothPattern @(PinaforeTypeSystem baseupdate)

qToPatternConstructor ::
       forall baseupdate t lt.
       ( ToListShimWit PinaforeShim (PinaforeType baseupdate 'Positive) lt
       , FromShimWit JMShim (PinaforeType baseupdate 'Negative) t
       )
    => (t -> Maybe (HList lt))
    -> QPatternConstructor baseupdate
qToPatternConstructor = toPatternConstructor (fromJMShimWit @(PinaforeType baseupdate 'Negative)) toListShimWit

qApplyPatternConstructor ::
       forall baseupdate.
       QPatternConstructor baseupdate
    -> QPattern baseupdate
    -> PinaforeSourceScoped baseupdate (QPatternConstructor baseupdate)
qApplyPatternConstructor = tsApplyPatternConstructor @(PinaforeTypeSystem baseupdate)

qSealPatternConstructor ::
       forall baseupdate m. MonadThrow ExpressionError m
    => QPatternConstructor baseupdate
    -> m (QPattern baseupdate)
qSealPatternConstructor = tsSealPatternConstructor @(PinaforeTypeSystem baseupdate)

qApplyAllPatternConstructor ::
       forall baseupdate.
       QPatternConstructor baseupdate
    -> [QPattern baseupdate]
    -> PinaforeSourceScoped baseupdate (QPatternConstructor baseupdate)
qApplyAllPatternConstructor pc [] = return pc
qApplyAllPatternConstructor pc (pat:pats) = do
    pc' <- qApplyPatternConstructor pc pat
    qApplyAllPatternConstructor pc' pats

qConstructPattern ::
       forall baseupdate.
       QPatternConstructor baseupdate
    -> [QPattern baseupdate]
    -> PinaforeSourceScoped baseupdate (QPattern baseupdate)
qConstructPattern pc pats = do
    pc' <- qApplyAllPatternConstructor pc pats
    qSealPatternConstructor pc'

qCase ::
       forall baseupdate.
       QExpr baseupdate
    -> [(QPattern baseupdate, QExpr baseupdate)]
    -> PinaforeSourceScoped baseupdate (QExpr baseupdate)
qCase = tsCase @(PinaforeTypeSystem baseupdate)

qFunctionPosWitness ::
       forall baseupdate a b.
       PinaforeShimWit baseupdate 'Negative a
    -> PinaforeShimWit baseupdate 'Positive b
    -> PinaforeShimWit baseupdate 'Positive (a -> b)
qFunctionPosWitness = tsFunctionPosShimWit @(PinaforeTypeSystem baseupdate)

qFunctionPosWitnesses ::
       ListType (PinaforeShimWit baseupdate 'Negative) a
    -> PinaforeShimWit baseupdate 'Positive b
    -> PinaforeShimWit baseupdate 'Positive (HList a -> b)
qFunctionPosWitnesses NilListType tb = mapShimWit (toEnhanced "poswitness" $ \ub -> ub ()) tb
qFunctionPosWitnesses (ConsListType ta la) tb =
    mapShimWit (toEnhanced "poswitness" $ \f a l -> f (a, l)) $ qFunctionPosWitness ta $ qFunctionPosWitnesses la tb

qCaseAbstract ::
       forall baseupdate.
       [(QPattern baseupdate, QExpr baseupdate)]
    -> PinaforeSourceScoped baseupdate (QExpr baseupdate)
qCaseAbstract = tsCaseAbstract @(PinaforeTypeSystem baseupdate)

qApplyExpr ::
       forall baseupdate. QExpr baseupdate -> QExpr baseupdate -> PinaforeSourceScoped baseupdate (QExpr baseupdate)
qApplyExpr exprf expra = tsApply @(PinaforeTypeSystem baseupdate) exprf expra

qApplyAllExpr :: QExpr baseupdate -> [QExpr baseupdate] -> PinaforeSourceScoped baseupdate (QExpr baseupdate)
qApplyAllExpr e [] = return e
qApplyAllExpr e (a:aa) = do
    e' <- qApplyExpr e a
    qApplyAllExpr e' aa

qEmptyList :: QExpr baseupdate
qEmptyList = qConstExpr ([] :: [BottomType])

qConsList :: QExpr baseupdate
qConsList = qConstExpr ((:) :: UVar "a" -> [UVar "a"] -> [UVar "a"])

qSequenceExpr :: [QExpr baseupdate] -> PinaforeSourceScoped baseupdate (QExpr baseupdate)
qSequenceExpr [] = return $ qEmptyList
qSequenceExpr (e:ee) = do
    ee' <- qSequenceExpr ee
    qApplyAllExpr qConsList [e, ee']

type QBindings baseupdate = TSBindings (PinaforeTypeSystem baseupdate)

qBindExpr :: forall baseupdate. Name -> QExpr baseupdate -> QBindings baseupdate
qBindExpr = singleBinding

qBindVal :: ToPinaforeType baseupdate t => Name -> t -> QBindings baseupdate
qBindVal name val = qBindExpr name $ qConstExpr val

qLetExpr ::
       forall baseupdate.
       Name
    -> QExpr baseupdate
    -> QExpr baseupdate
    -> PinaforeSourceScoped baseupdate (QExpr baseupdate)
qLetExpr name exp body = tsLet @(PinaforeTypeSystem baseupdate) name exp body

qUncheckedBindingsComponentLetExpr ::
       forall baseupdate. QBindings baseupdate -> PinaforeSourceScoped baseupdate (StrictMap Name (QExpr baseupdate))
qUncheckedBindingsComponentLetExpr = tsUncheckedComponentLet @(PinaforeTypeSystem baseupdate)

qValuesLetExpr :: forall baseupdate. StrictMap Name (QValue baseupdate) -> StrictMap Name (QExpr baseupdate)
qValuesLetExpr = tsValuesLet @(PinaforeTypeSystem baseupdate)

qEvalExpr ::
       forall baseupdate m. MonadThrow ExpressionError m
    => QExpr baseupdate
    -> m (QValue baseupdate)
qEvalExpr expr = tsEval @(PinaforeTypeSystem baseupdate) expr

typedAnyToPinaforeVal ::
       forall baseupdate t. FromPinaforeType baseupdate t
    => QValue baseupdate
    -> PinaforeSourceScoped baseupdate t
typedAnyToPinaforeVal = tsAnyToVal @(PinaforeTypeSystem baseupdate) fromJMShimWit

qSubsumeExpr ::
       forall baseupdate.
       AnyW (PinaforeShimWit baseupdate 'Positive)
    -> PinaforeExpression baseupdate
    -> PinaforeSourceScoped baseupdate (PinaforeExpression baseupdate)
qSubsumeExpr t expr = tsSubsume @(PinaforeTypeSystem baseupdate) t expr
