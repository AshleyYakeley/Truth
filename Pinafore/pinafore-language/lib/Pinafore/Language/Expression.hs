{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Expression where

import Pinafore.Language.Convert
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Shim
import Pinafore.Language.Type
import Pinafore.Language.Var
import Pinafore.Language.VarID
import Pinafore.Markdown
import Shapes

type QExpr = TSSealedExpression PinaforeTypeSystem

type QPattern = TSSealedPattern PinaforeTypeSystem

type QPatternConstructor = TSPatternConstructor PinaforeTypeSystem

type QValue = TSValue PinaforeTypeSystem

qConstExprAny :: QValue -> QExpr
qConstExprAny = tsConst @PinaforeTypeSystem

qConstExpr ::
       forall a. HasPinaforeType 'Positive a
    => a
    -> QExpr
qConstExpr a = qConstExprAny $ jmToValue a

qVarExpr :: VarID -> QExpr
qVarExpr name = tsVar @PinaforeTypeSystem name

qName :: ReferenceName -> PinaforeInterpreter QExpr
qName name = do
    mexpr <- lookupLetBinding name
    case mexpr of
        Just (Right expr) -> return expr
        Just (Left v) -> return $ qVarExpr v
        Nothing -> do
            spos <- paramAsk sourcePosParam
            return $ qVarExpr $ mkBadVarID spos name

qAbstractExpr :: VarID -> QExpr -> PinaforeInterpreter QExpr
qAbstractExpr name expr = tsAbstract @PinaforeTypeSystem name expr

qAbstractsExpr :: [VarID] -> QExpr -> PinaforeInterpreter QExpr
qAbstractsExpr [] e = return e
qAbstractsExpr (n:nn) e = do
    e' <- qAbstractsExpr nn e
    qAbstractExpr n e'

qVarPattern :: VarID -> QPattern
qVarPattern = tsVarPattern @PinaforeTypeSystem

qAnyPattern :: QPattern
qAnyPattern = tsAnyPattern @PinaforeTypeSystem

qBothPattern :: QPattern -> QPattern -> PinaforeInterpreter QPattern
qBothPattern = tsBothPattern @PinaforeTypeSystem

qToPatternConstructor ::
       forall t lt.
       ( ToListShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) lt
       , FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) t
       )
    => (t -> Maybe (ListProduct lt))
    -> QPatternConstructor
qToPatternConstructor =
    toPatternConstructor (fromPolarShimWit @Type @(PinaforePolyShim Type) @(PinaforeType 'Negative)) toListShimWit

qApplyPatternConstructor :: QPatternConstructor -> QPattern -> PinaforeInterpreter (QPatternConstructor)
qApplyPatternConstructor = tsApplyPatternConstructor @PinaforeTypeSystem

qSealPatternConstructor ::
       forall m. MonadThrow ExpressionError m
    => QPatternConstructor
    -> m QPattern
qSealPatternConstructor = tsSealPatternConstructor @PinaforeTypeSystem

qApplyAllPatternConstructor :: QPatternConstructor -> [QPattern] -> PinaforeInterpreter (QPatternConstructor)
qApplyAllPatternConstructor pc [] = return pc
qApplyAllPatternConstructor pc (pat:pats) = do
    pc' <- qApplyPatternConstructor pc pat
    qApplyAllPatternConstructor pc' pats

qConstructPattern :: QPatternConstructor -> [QPattern] -> PinaforeInterpreter QPattern
qConstructPattern pc pats = do
    pc' <- qApplyAllPatternConstructor pc pats
    qSealPatternConstructor pc'

qCase :: QExpr -> [(QPattern, QExpr)] -> PinaforeInterpreter QExpr
qCase = tsCase @PinaforeTypeSystem

qFunctionPosWitness ::
       forall a b. PinaforeShimWit 'Negative a -> PinaforeShimWit 'Positive b -> PinaforeShimWit 'Positive (a -> b)
qFunctionPosWitness = tsFunctionPosShimWit @PinaforeTypeSystem

qFunctionPosWitnesses ::
       ListType (PinaforeShimWit 'Negative) a
    -> PinaforeShimWit 'Positive b
    -> PinaforeShimWit 'Positive (ListProduct a -> b)
qFunctionPosWitnesses NilListType tb = mapPosShimWit (functionToShim "poswitness" $ \ub -> ub ()) tb
qFunctionPosWitnesses (ConsListType ta la) tb =
    mapPosShimWit (functionToShim "poswitness" $ \f a l -> f (a, l)) $
    qFunctionPosWitness ta $ qFunctionPosWitnesses la tb

qCaseAbstract :: [(QPattern, QExpr)] -> PinaforeInterpreter QExpr
qCaseAbstract = tsCaseAbstract @PinaforeTypeSystem

qApplyExpr :: QExpr -> QExpr -> PinaforeInterpreter QExpr
qApplyExpr exprf expra = tsApply @PinaforeTypeSystem exprf expra

qApplyAllExpr :: QExpr -> [QExpr] -> PinaforeInterpreter QExpr
qApplyAllExpr e [] = return e
qApplyAllExpr e (a:aa) = do
    e' <- qApplyExpr e a
    qApplyAllExpr e' aa

qEmptyList :: QExpr
qEmptyList = qConstExpr $ [] @BottomType

qConsList :: QExpr
qConsList = qConstExpr $ (:|) @A

qSequenceExpr :: [QExpr] -> PinaforeInterpreter QExpr
qSequenceExpr [] = return $ qEmptyList
qSequenceExpr (e:ee) = do
    ee' <- qSequenceExpr ee
    qApplyAllExpr qConsList [e, ee']

type QBinding = Binding PinaforeTypeSystem

qBindExpr :: VarID -> Markdown -> Maybe (Some (PinaforeType 'Positive)) -> QExpr -> QBinding
qBindExpr = tsSingleBinding @PinaforeTypeSystem

qSubsumeExpr :: Some (PinaforeType 'Positive) -> QExpr -> PinaforeInterpreter QExpr
qSubsumeExpr = tsSubsumeExpression @PinaforeTypeSystem

qLetExpr :: VarID -> QExpr -> QExpr -> PinaforeInterpreter QExpr
qLetExpr name exp body = tsLet @PinaforeTypeSystem name exp body

qUncheckedBindingsRecursiveLetExpr :: [QBinding] -> PinaforeInterpreter (Map VarID (Markdown, QExpr))
qUncheckedBindingsRecursiveLetExpr = tsUncheckedRecursiveLet @PinaforeTypeSystem

qBindingSequentialLetExpr :: QBinding -> PinaforeInterpreter (Map VarID (Markdown, QExpr))
qBindingSequentialLetExpr = tsSequentialLet @PinaforeTypeSystem

qEvalExpr ::
       forall m. MonadThrow ExpressionError m
    => QExpr
    -> m QValue
qEvalExpr expr = tsEval @PinaforeTypeSystem expr

typedAnyToVal :: forall t. PinaforeShimWit 'Negative t -> QValue -> PinaforeInterpreter t
typedAnyToVal = tsUnifyValueTo @PinaforeTypeSystem

typedAnyToPinaforeVal ::
       forall t. HasPinaforeType 'Negative t
    => QValue
    -> PinaforeInterpreter t
typedAnyToPinaforeVal = tsUnifyValue @PinaforeTypeSystem

-- | for debugging
rigidTypedAnyToPinaforeVal ::
       forall t. HasPinaforeType 'Negative t
    => QValue
    -> PinaforeInterpreter t
rigidTypedAnyToPinaforeVal = tsUnifyRigidValue @PinaforeTypeSystem
