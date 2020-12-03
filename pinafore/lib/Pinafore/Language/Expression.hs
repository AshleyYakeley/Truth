{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Expression
    ( module Pinafore.Language.Expression
    , PinaforeInterpreter
    ) where

import Pinafore.Language.Convert
import Pinafore.Language.Name
import Pinafore.Language.Shim
import Pinafore.Language.Type
import Pinafore.Language.Var
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

qAbstractExpr :: Name -> QExpr -> PinaforeSourceInterpreter QExpr
qAbstractExpr name expr = tsAbstract @PinaforeTypeSystem name expr

qAbstractsExpr :: [Name] -> QExpr -> PinaforeSourceInterpreter QExpr
qAbstractsExpr [] e = return e
qAbstractsExpr (n:nn) e = do
    e' <- qAbstractsExpr nn e
    qAbstractExpr n e'

qVarPattern :: Name -> QPattern
qVarPattern = tsVarPattern @PinaforeTypeSystem

qAnyPattern :: QPattern
qAnyPattern = tsAnyPattern @PinaforeTypeSystem

qBothPattern :: QPattern -> QPattern -> PinaforeSourceInterpreter QPattern
qBothPattern = tsBothPattern @PinaforeTypeSystem

qToPatternConstructor ::
       forall t lt.
       ( ToListShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) lt
       , FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) t
       )
    => (t -> Maybe (HList lt))
    -> QPatternConstructor
qToPatternConstructor =
    toPatternConstructor (fromShimWit @Type @(PinaforePolyShim Type) @(PinaforeType 'Negative)) toListShimWit

qApplyPatternConstructor :: QPatternConstructor -> QPattern -> PinaforeSourceInterpreter (QPatternConstructor)
qApplyPatternConstructor = tsApplyPatternConstructor @PinaforeTypeSystem

qSealPatternConstructor ::
       forall m. MonadThrow ExpressionError m
    => QPatternConstructor
    -> m QPattern
qSealPatternConstructor = tsSealPatternConstructor @PinaforeTypeSystem

qApplyAllPatternConstructor :: QPatternConstructor -> [QPattern] -> PinaforeSourceInterpreter (QPatternConstructor)
qApplyAllPatternConstructor pc [] = return pc
qApplyAllPatternConstructor pc (pat:pats) = do
    pc' <- qApplyPatternConstructor pc pat
    qApplyAllPatternConstructor pc' pats

qConstructPattern :: QPatternConstructor -> [QPattern] -> PinaforeSourceInterpreter QPattern
qConstructPattern pc pats = do
    pc' <- qApplyAllPatternConstructor pc pats
    qSealPatternConstructor pc'

qCase :: QExpr -> [(QPattern, QExpr)] -> PinaforeSourceInterpreter QExpr
qCase = tsCase @PinaforeTypeSystem

qFunctionPosWitness ::
       forall a b. PinaforeShimWit 'Negative a -> PinaforeShimWit 'Positive b -> PinaforeShimWit 'Positive (a -> b)
qFunctionPosWitness = tsFunctionPosShimWit @PinaforeTypeSystem

qFunctionPosWitnesses ::
       ListType (PinaforeShimWit 'Negative) a -> PinaforeShimWit 'Positive b -> PinaforeShimWit 'Positive (HList a -> b)
qFunctionPosWitnesses NilListType tb = mapPosShimWit (functionToShim "poswitness" $ \ub -> ub ()) tb
qFunctionPosWitnesses (ConsListType ta la) tb =
    mapPosShimWit (functionToShim "poswitness" $ \f a l -> f (a, l)) $
    qFunctionPosWitness ta $ qFunctionPosWitnesses la tb

qCaseAbstract :: [(QPattern, QExpr)] -> PinaforeSourceInterpreter QExpr
qCaseAbstract = tsCaseAbstract @PinaforeTypeSystem

qApplyExpr :: QExpr -> QExpr -> PinaforeSourceInterpreter QExpr
qApplyExpr exprf expra = tsApply @PinaforeTypeSystem exprf expra

qApplyAllExpr :: QExpr -> [QExpr] -> PinaforeSourceInterpreter QExpr
qApplyAllExpr e [] = return e
qApplyAllExpr e (a:aa) = do
    e' <- qApplyExpr e a
    qApplyAllExpr e' aa

qEmptyList :: QExpr
qEmptyList = qConstExpr ([] :: [BottomType])

qConsList :: QExpr
qConsList = qConstExpr ((:) :: A -> [A] -> [A])

qSequenceExpr :: [QExpr] -> PinaforeSourceInterpreter QExpr
qSequenceExpr [] = return $ qEmptyList
qSequenceExpr (e:ee) = do
    ee' <- qSequenceExpr ee
    qApplyAllExpr qConsList [e, ee']

type QBindings = Bindings PinaforeTypeSystem

qBindExpr :: Name -> Maybe (AnyW (PinaforeType 'Positive)) -> QExpr -> QBindings
qBindExpr = tsSingleBinding @PinaforeTypeSystem

qBindVal :: ToPinaforeType t => Name -> t -> QBindings
qBindVal name val = qBindExpr name Nothing $ qConstExpr val

qLetExpr :: Name -> QExpr -> QExpr -> PinaforeSourceInterpreter QExpr
qLetExpr name exp body = tsLet @PinaforeTypeSystem name exp body

qUncheckedBindingsComponentLetExpr :: QBindings -> PinaforeSourceInterpreter (Map Name QExpr)
qUncheckedBindingsComponentLetExpr = tsUncheckedComponentLet @PinaforeTypeSystem

qEvalExpr ::
       forall m. MonadThrow ExpressionError m
    => QExpr
    -> m QValue
qEvalExpr expr = tsEval @PinaforeTypeSystem expr

typedAnyToPinaforeVal ::
       forall t. FromPinaforeType t
    => QValue
    -> PinaforeSourceInterpreter t
typedAnyToPinaforeVal = tsUnifyValue @PinaforeTypeSystem fromJMShimWit

qSubsumeExpr :: AnyW (PinaforeShimWit 'Positive) -> PinaforeExpression -> PinaforeSourceInterpreter PinaforeExpression
qSubsumeExpr t expr = tsSubsumeExpression @PinaforeTypeSystem t expr
