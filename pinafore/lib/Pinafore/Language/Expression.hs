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
import Pinafore.Language.Shim
import Pinafore.Language.TypeSystem
import Pinafore.Language.TypeSystem.Subsume ()
import Shapes

type QExpr = TSSealedExpression PinaforeTypeSystem

type QPattern = TSSealedPattern PinaforeTypeSystem

type QPatternConstructor = TSPatternConstructor PinaforeTypeSystem

type QValue = TSValue PinaforeTypeSystem

qConstExprAny :: QValue -> QExpr
qConstExprAny = tsConst @PinaforeTypeSystem

qConstExpr ::
       forall a. ToPinaforeType a
    => a
    -> QExpr
qConstExpr a = qConstExprAny $ jmToValue a

qVarExpr :: Name -> QExpr
qVarExpr name = tsVar @PinaforeTypeSystem name

qAbstractExpr :: Name -> QExpr -> PinaforeSourceScoped QExpr
qAbstractExpr name expr = tsAbstract @PinaforeTypeSystem name expr

qAbstractsExpr :: [Name] -> QExpr -> PinaforeSourceScoped QExpr
qAbstractsExpr [] e = return e
qAbstractsExpr (n:nn) e = do
    e' <- qAbstractsExpr nn e
    qAbstractExpr n e'

qVarPattern :: Name -> QPattern
qVarPattern = tsVarPattern @PinaforeTypeSystem

qAnyPattern :: QPattern
qAnyPattern = tsAnyPattern @PinaforeTypeSystem

qBothPattern :: QPattern -> QPattern -> PinaforeSourceScoped QPattern
qBothPattern = tsBothPattern @PinaforeTypeSystem

qToPatternConstructor ::
       forall t lt.
       ( ToListShimWit (PinaforeShim Type) (PinaforeType 'Positive) lt
       , FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) t
       )
    => (t -> Maybe (HList lt))
    -> QPatternConstructor
qToPatternConstructor =
    toPatternConstructor (fromShimWit @Type @(PinaforeShim Type) @(PinaforeType 'Negative)) toListShimWit

qApplyPatternConstructor :: QPatternConstructor -> QPattern -> PinaforeSourceScoped (QPatternConstructor)
qApplyPatternConstructor = tsApplyPatternConstructor @PinaforeTypeSystem

qSealPatternConstructor ::
       forall m. MonadThrow ExpressionError m
    => QPatternConstructor
    -> m QPattern
qSealPatternConstructor = tsSealPatternConstructor @PinaforeTypeSystem

qApplyAllPatternConstructor :: QPatternConstructor -> [QPattern] -> PinaforeSourceScoped (QPatternConstructor)
qApplyAllPatternConstructor pc [] = return pc
qApplyAllPatternConstructor pc (pat:pats) = do
    pc' <- qApplyPatternConstructor pc pat
    qApplyAllPatternConstructor pc' pats

qConstructPattern :: QPatternConstructor -> [QPattern] -> PinaforeSourceScoped QPattern
qConstructPattern pc pats = do
    pc' <- qApplyAllPatternConstructor pc pats
    qSealPatternConstructor pc'

qCase :: QExpr -> [(QPattern, QExpr)] -> PinaforeSourceScoped QExpr
qCase = tsCase @PinaforeTypeSystem

qFunctionPosWitness ::
       forall a b.
       PinaforeTypeShimWit 'Negative a
    -> PinaforeTypeShimWit 'Positive b
    -> PinaforeTypeShimWit 'Positive (a -> b)
qFunctionPosWitness = tsFunctionPosShimWit @PinaforeTypeSystem

qFunctionPosWitnesses ::
       ListType (PinaforeTypeShimWit 'Negative) a
    -> PinaforeTypeShimWit 'Positive b
    -> PinaforeTypeShimWit 'Positive (HList a -> b)
qFunctionPosWitnesses NilListType tb = mapPosShimWit (toEnhanced "poswitness" $ \ub -> ub ()) tb
qFunctionPosWitnesses (ConsListType ta la) tb =
    mapPosShimWit (toEnhanced "poswitness" $ \f a l -> f (a, l)) $ qFunctionPosWitness ta $ qFunctionPosWitnesses la tb

qCaseAbstract :: [(QPattern, QExpr)] -> PinaforeSourceScoped QExpr
qCaseAbstract = tsCaseAbstract @PinaforeTypeSystem

qApplyExpr :: QExpr -> QExpr -> PinaforeSourceScoped QExpr
qApplyExpr exprf expra = tsApply @PinaforeTypeSystem exprf expra

qApplyAllExpr :: QExpr -> [QExpr] -> PinaforeSourceScoped QExpr
qApplyAllExpr e [] = return e
qApplyAllExpr e (a:aa) = do
    e' <- qApplyExpr e a
    qApplyAllExpr e' aa

qEmptyList :: QExpr
qEmptyList = qConstExpr ([] :: [BottomType])

qConsList :: QExpr
qConsList = qConstExpr ((:) :: UVar "a" -> [UVar "a"] -> [UVar "a"])

qSequenceExpr :: [QExpr] -> PinaforeSourceScoped QExpr
qSequenceExpr [] = return $ qEmptyList
qSequenceExpr (e:ee) = do
    ee' <- qSequenceExpr ee
    qApplyAllExpr qConsList [e, ee']

type QBindings = TSBindings PinaforeTypeSystem

qBindExpr :: Name -> QExpr -> QBindings
qBindExpr = singleBinding

qBindVal :: ToPinaforeType t => Name -> t -> QBindings
qBindVal name val = qBindExpr name $ qConstExpr val

qLetExpr :: Name -> QExpr -> QExpr -> PinaforeSourceScoped QExpr
qLetExpr name exp body = tsLet @PinaforeTypeSystem name exp body

qUncheckedBindingsComponentLetExpr :: QBindings -> PinaforeSourceScoped (Map Name QExpr)
qUncheckedBindingsComponentLetExpr = tsUncheckedComponentLet @PinaforeTypeSystem

qValuesLetExpr :: Map Name QValue -> Map Name QExpr
qValuesLetExpr = tsValuesLet @PinaforeTypeSystem

qEvalExpr ::
       forall m. MonadThrow ExpressionError m
    => QExpr
    -> m QValue
qEvalExpr expr = tsEval @PinaforeTypeSystem expr

typedAnyToPinaforeVal ::
       forall t. FromPinaforeType t
    => QValue
    -> PinaforeSourceScoped t
typedAnyToPinaforeVal = tsAnyToVal @PinaforeTypeSystem fromJMShimWit

qSubsumeExpr :: AnyW (PinaforeTypeShimWit 'Positive) -> PinaforeExpression -> PinaforeSourceScoped PinaforeExpression
qSubsumeExpr t expr = tsSubsume @PinaforeTypeSystem t expr
