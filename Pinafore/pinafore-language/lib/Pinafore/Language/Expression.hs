{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Expression where

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Interpreter
import Pinafore.Language.Type
import Pinafore.Language.Var
import Pinafore.Language.VarID

qToValue ::
    forall a.
    HasQType QPolyShim 'Positive a =>
    a ->
    QValue
qToValue = jmToValue

qConstValue :: QValue -> QExpression
qConstValue = tsConst @QTypeSystem

qConst ::
    forall a.
    HasQType QPolyShim 'Positive a =>
    a ->
    QExpression
qConst a = qConstValue $ qToValue a

qVar :: VarID -> QExpression
qVar name = tsVar @QTypeSystem name

qSubstitute :: (VarID -> Maybe QExpression) -> QExpression -> QInterpreter QExpression
qSubstitute = tsSubstitute @QTypeSystem

qImply :: [(ImplicitName, QExpression)] -> QExpression -> QInterpreter QExpression
qImply substs expr = let
    substf (ImplicitVarID vn) = lookup vn substs
    substf _ = Nothing
    in qSubstitute substf expr

qAbstractExpr :: VarID -> QExpression -> QInterpreter QExpression
qAbstractExpr = tsAbstract @QTypeSystem

qAbstractsExpr :: [VarID] -> QExpression -> QInterpreter QExpression
qAbstractsExpr [] e = return e
qAbstractsExpr (n : nn) e = do
    e' <- qAbstractsExpr nn e
    qAbstractExpr n e'

qPolyAbstractF ::
    forall p q. VarID -> QShimWit 'Positive p -> QFExpression ((->) q) -> QInterpreter (QFExpression ((->) (p, q)))
qPolyAbstractF = tsPolyAbstractF @QTypeSystem

qSimplify ::
    forall a.
    TSMappable QTypeSystem a =>
    a ->
    QInterpreter a
qSimplify = tsSimplify @QTypeSystem

qVarPattern :: VarID -> QPattern
qVarPattern = tsVarPattern @QTypeSystem

qAnyPattern :: QPattern
qAnyPattern = tsAnyPattern @QTypeSystem

qBothPattern :: QPattern -> QPattern -> QInterpreter QPattern
qBothPattern = tsBothPattern @QTypeSystem

qToPatternConstructor ::
    forall t lt.
    (ToListShimWit QShim (QType 'Positive) lt, FromPolarShimWit QShim (QType 'Negative) t) =>
    QPurityFunction t lt ->
    QPatternConstructor
qToPatternConstructor tml = toPatternConstructor (fromPolarShimWit @Type @QShim @(QType 'Negative)) toListShimWit tml

qApplyPatternConstructor :: QPatternConstructor -> QPattern -> QInterpreter (QPatternConstructor)
qApplyPatternConstructor = tsApplyPatternConstructor @QTypeSystem

qSealPatternConstructor ::
    forall m.
    MonadThrow PatternError m =>
    QPatternConstructor ->
    m QPattern
qSealPatternConstructor = tsSealPatternConstructor @QTypeSystem

qApplyAllPatternConstructor :: QPatternConstructor -> [QPattern] -> QInterpreter (QPatternConstructor)
qApplyAllPatternConstructor pc [] = return pc
qApplyAllPatternConstructor pc (pat : pats) = do
    pc' <- qApplyPatternConstructor pc pat
    qApplyAllPatternConstructor pc' pats

qConstructPattern :: QPatternConstructor -> [QPattern] -> QInterpreter QPattern
qConstructPattern pc pats = do
    pc' <- qApplyAllPatternConstructor pc pats
    qSealPatternConstructor pc'

qPartialExpressionSumList :: [QPartialExpression] -> QInterpreter QPartialExpression
qPartialExpressionSumList = tsPartialExpressionSumList @QTypeSystem

qLambdaPatternMatch :: VarID -> QPattern -> QMatch
qLambdaPatternMatch = tsLambdaPatternMatch @QTypeSystem

qExpressionPatternMatch :: QExpression -> QPattern -> QInterpreter QMatch
qExpressionPatternMatch = tsExpressionPatternMatch @QTypeSystem

qMatchGate :: QMatch -> QPartialExpression -> QInterpreter QPartialExpression
qMatchGate = tsMatchGate @QTypeSystem

qMatchBindings :: QMatch -> [(VarID, QExpression)]
qMatchBindings = tsMatchBindings @QTypeSystem

qFunctionPosWitness :: forall a b. QShimWit 'Negative a -> QShimWit 'Positive b -> QShimWit 'Positive (a -> b)
qFunctionPosWitness = tsFunctionPosShimWit @QTypeSystem

qFunctionPosWitnesses ::
    ListType (QShimWit 'Negative) a -> QShimWit 'Positive b -> QShimWit 'Positive (ListProduct a -> b)
qFunctionPosWitnesses NilListType tb = mapPosShimWit (functionToShim "poswitness" $ \ub -> ub ()) tb
qFunctionPosWitnesses (ConsListType ta la) tb =
    mapPosShimWit (functionToShim "poswitness" $ \f a l -> f (a, l))
        $ qFunctionPosWitness ta
        $ qFunctionPosWitnesses la tb

qApplyExpr :: QExpression -> QExpression -> QInterpreter QExpression
qApplyExpr exprf expra = tsApply @QTypeSystem exprf expra

qApplyAllExpr :: QExpression -> [QExpression] -> QInterpreter QExpression
qApplyAllExpr e [] = return e
qApplyAllExpr e (a : aa) = do
    e' <- qApplyExpr e a
    qApplyAllExpr e' aa

qEmptyList :: QExpression
qEmptyList = qConst $ [] @BottomType

qConsList :: QExpression
qConsList = qConst $ (:|) @A

qSequenceExpr :: [QExpression] -> QInterpreter QExpression
qSequenceExpr [] = return $ qEmptyList
qSequenceExpr (e : ee) = do
    ee' <- qSequenceExpr ee
    qApplyAllExpr qConsList [e, ee']

qBindExpr :: VarID -> (QExpression -> DefDoc) -> Maybe (Some (QType 'Positive)) -> QExpression -> QBinding
qBindExpr = tsSingleBinding @QTypeSystem

qSubsumeExpr :: Some (QType 'Positive) -> QExpression -> QInterpreter QExpression
qSubsumeExpr = tsSubsumeExpression @QTypeSystem

qSubsumeFExpr :: Functor f => Some (QType 'Positive) -> QFExpression f -> QInterpreter (QFExpression f)
qSubsumeFExpr = tsSubsumeFExpression @QTypeSystem

qLetExpr :: VarID -> QExpression -> QExpression -> QInterpreter QExpression
qLetExpr name exp body = tsLet @QTypeSystem name exp body

qUncheckedBindingsRecursiveLetExpr :: [QBinding] -> QInterpreter (Map VarID (DefDoc, QExpression))
qUncheckedBindingsRecursiveLetExpr = tsUncheckedRecursiveLet @QTypeSystem

qBindingSequentialLetExpr :: QBinding -> QInterpreter (Map VarID (DefDoc, QExpression))
qBindingSequentialLetExpr = tsSequentialLet @QTypeSystem

qEvalExpr ::
    forall m.
    MonadThrow (ExpressionError QVarWit) m =>
    QExpression ->
    m QValue
qEvalExpr expr = tsEval @QTypeSystem expr

qEvalExprMaybe :: QExpression -> Maybe QValue
qEvalExprMaybe expr = tsEvalMaybe @QTypeSystem expr

qUnifyRigid :: forall a b. QShimWit 'Positive a -> QShimWit 'Negative b -> QInterpreter (QShim a b)
qUnifyRigid = tsUnifyRigid @QTypeSystem

qUnifyValueTo :: forall t. QShimWit 'Negative t -> QValue -> QInterpreter t
qUnifyValueTo = tsUnifyValueTo @QTypeSystem

qValue ::
    forall t.
    HasQType QPolyShim 'Positive t =>
    t ->
    QValue
qValue v = MkSomeOf toPolarShimWit v

qUnifyExpressionToOpen :: forall t. QShimWit 'Negative t -> QExpression -> QInterpreter (QOpenExpression t)
qUnifyExpressionToOpen = tsUnifyExpressionTo @QTypeSystem

qSubsumeExpressionToOpen :: forall t. QType 'Positive t -> QExpression -> QInterpreter (QOpenExpression t)
qSubsumeExpressionToOpen = tsSubsumeExpressionTo @QTypeSystem mempty

qSubsumeExpressionToOpenWit :: forall t. QIsoShimWit 'Positive t -> QExpression -> QInterpreter (QOpenExpression t)
qSubsumeExpressionToOpenWit (MkShimWit t iconv) expr = do
    oexpr <- qSubsumeExpressionToOpen t expr
    return $ fmap (shimToFunction $ polarPolyIsoNegative iconv) oexpr

qUnifyValue ::
    forall t.
    HasQType QPolyShim 'Negative t =>
    QValue ->
    QInterpreter t
qUnifyValue = tsUnifyValue @QTypeSystem

qExactValue :: QType 'Positive t -> QValue -> Maybe t
qExactValue wt (MkSomeOf (MkShimWit wt' (MkPolarShim conv)) v) = do
    Refl <- testEquality wt wt'
    return $ shimToFunction conv v

qUnifyF :: forall f. (forall t. QShimWit 'Negative t -> QShimWit 'Negative (f t)) -> QValue -> QInterpreter (QValueF f)
qUnifyF = tsUnifyF @QTypeSystem

-- | for debugging
qUnifyRigidValue ::
    forall t.
    HasQType QPolyShim 'Negative t =>
    QValue ->
    QInterpreter t
qUnifyRigidValue = tsUnifyRigidValue @QTypeSystem

-- | for debugging
qUnifyValueToFree ::
    forall t.
    HasQType QPolyShim 'Negative t =>
    QValue ->
    QInterpreter t
qUnifyValueToFree = tsUnifyValueToFree @QTypeSystem
