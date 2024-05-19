{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Expression where

import Pinafore.Language.Convert
import Pinafore.Language.DefDoc
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Shim
import Pinafore.Language.Type
import Pinafore.Language.Var
import Pinafore.Language.VarID
import Shapes

qConstExprAny :: QValue -> QExpression
qConstExprAny = tsConst @QTypeSystem

qConstExpr ::
       forall a. HasQType 'Positive a
    => a
    -> QExpression
qConstExpr a = qConstExprAny $ jmToValue a

qVarExpr :: VarID -> QExpression
qVarExpr name = tsVar @QTypeSystem name

qNameWith :: FullNameRef -> QInterpreter QExpression -> QInterpreter QExpression
qNameWith name qidefexpr = do
    mexpr <- lookupMaybeValue name
    case mexpr of
        Just expr -> return expr
        _ -> qidefexpr

qName :: FullNameRef -> QInterpreter QExpression
qName name =
    qNameWith name $ do
        spos <- paramAsk sourcePosParam
        return $ qVarExpr $ mkBadVarID spos name

qNameWithDefault :: Maybe QExpression -> FullNameRef -> QInterpreter QExpression
qNameWithDefault (Just defexpr) name = qNameWith name $ return defexpr
qNameWithDefault Nothing name = qName name

qSubstitute :: (VarID -> Maybe QExpression) -> QExpression -> QInterpreter QExpression
qSubstitute = tsSubstitute @QTypeSystem

qAbstractExpr :: VarID -> QExpression -> QInterpreter QExpression
qAbstractExpr = tsAbstract @QTypeSystem

qAbstractsExpr :: [VarID] -> QExpression -> QInterpreter QExpression
qAbstractsExpr [] e = return e
qAbstractsExpr (n:nn) e = do
    e' <- qAbstractsExpr nn e
    qAbstractExpr n e'

qSimplify ::
       forall a. TSMappable QTypeSystem a
    => a
    -> QInterpreter a
qSimplify = tsSimplify @QTypeSystem

qVarPattern :: VarID -> QPattern
qVarPattern = tsVarPattern @QTypeSystem

qAnyPattern :: QPattern
qAnyPattern = tsAnyPattern @QTypeSystem

qBothPattern :: QPattern -> QPattern -> QInterpreter QPattern
qBothPattern = tsBothPattern @QTypeSystem

qToPatternConstructor ::
       forall t lt.
       (ToListShimWit (QPolyShim Type) (QType 'Positive) lt, FromPolarShimWit (QPolyShim Type) (QType 'Negative) t)
    => PurityFunction Maybe t (ListProduct lt)
    -> QPatternConstructor
qToPatternConstructor tml =
    toExpressionPatternConstructor $
    toPatternConstructor (fromPolarShimWit @Type @(QPolyShim Type) @(QType 'Negative)) toListShimWit tml

qApplyPatternConstructor :: QPatternConstructor -> QPattern -> QInterpreter (QPatternConstructor)
qApplyPatternConstructor = tsApplyPatternConstructor @QTypeSystem

qSealPatternConstructor ::
       forall m. MonadThrow PatternError m
    => QPatternConstructor
    -> m QPattern
qSealPatternConstructor = tsSealPatternConstructor @QTypeSystem

qApplyAllPatternConstructor :: QPatternConstructor -> [QPattern] -> QInterpreter (QPatternConstructor)
qApplyAllPatternConstructor pc [] = return pc
qApplyAllPatternConstructor pc (pat:pats) = do
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
    mapPosShimWit (functionToShim "poswitness" $ \f a l -> f (a, l)) $
    qFunctionPosWitness ta $ qFunctionPosWitnesses la tb

qApplyExpr :: QExpression -> QExpression -> QInterpreter QExpression
qApplyExpr exprf expra = tsApply @QTypeSystem exprf expra

qApplyAllExpr :: QExpression -> [QExpression] -> QInterpreter QExpression
qApplyAllExpr e [] = return e
qApplyAllExpr e (a:aa) = do
    e' <- qApplyExpr e a
    qApplyAllExpr e' aa

qEmptyList :: QExpression
qEmptyList = qConstExpr $ [] @BottomType

qConsList :: QExpression
qConsList = qConstExpr $ (:|) @A

qSequenceExpr :: [QExpression] -> QInterpreter QExpression
qSequenceExpr [] = return $ qEmptyList
qSequenceExpr (e:ee) = do
    ee' <- qSequenceExpr ee
    qApplyAllExpr qConsList [e, ee']

qBindExpr :: VarID -> DefDoc -> Maybe (Some (QType 'Positive)) -> QExpression -> QBinding
qBindExpr = tsSingleBinding @QTypeSystem

qSubsumeExpr :: Some (QType 'Positive) -> QExpression -> QInterpreter QExpression
qSubsumeExpr = tsSubsumeExpression @QTypeSystem

qLetExpr :: VarID -> QExpression -> QExpression -> QInterpreter QExpression
qLetExpr name exp body = tsLet @QTypeSystem name exp body

qUncheckedBindingsRecursiveLetExpr :: [QBinding] -> QInterpreter (Map VarID (DefDoc, QExpression))
qUncheckedBindingsRecursiveLetExpr = tsUncheckedRecursiveLet @QTypeSystem

qBindingSequentialLetExpr :: QBinding -> QInterpreter (Map VarID (DefDoc, QExpression))
qBindingSequentialLetExpr = tsSequentialLet @QTypeSystem

qEvalExpr ::
       forall m. MonadThrow (NamedExpressionError VarID (QShimWit 'Negative)) m
    => QExpression
    -> m QValue
qEvalExpr expr = tsEval @QTypeSystem expr

qUnifyValueTo :: forall t. QShimWit 'Negative t -> QValue -> QInterpreter t
qUnifyValueTo = tsUnifyValueTo @QTypeSystem

qValue ::
       forall t. HasQType 'Positive t
    => t
    -> QValue
qValue v = MkSomeOf toPolarShimWit v

typedUnifyExpressionToOpen :: forall t. QShimWit 'Negative t -> QExpression -> QInterpreter (QOpenExpression t)
typedUnifyExpressionToOpen = tsUnifyExpressionTo @QTypeSystem

typedSubsumeExpressionToOpen ::
       forall t. FiniteSet SomeTypeVarT -> QType 'Positive t -> QExpression -> QInterpreter (QOpenExpression t)
typedSubsumeExpressionToOpen = tsSubsumeExpressionTo @QTypeSystem

qUnifyValue ::
       forall t. HasQType 'Negative t
    => QValue
    -> QInterpreter t
qUnifyValue = tsUnifyValue @QTypeSystem

qExactValue :: QType 'Positive t -> QValue -> Maybe t
qExactValue wt (MkSomeOf (MkShimWit wt' (MkPolarShim conv)) v) = do
    Refl <- testEquality wt wt'
    return $ shimToFunction conv v

qUnifyF :: forall f. (forall t. QShimWit 'Negative t -> QShimWit 'Negative (f t)) -> QValue -> QInterpreter (QValueF f)
qUnifyF = tsUnifyF @QTypeSystem

-- | for debugging
qUnifyRigidValue ::
       forall t. HasQType 'Negative t
    => QValue
    -> QInterpreter t
qUnifyRigidValue = tsUnifyRigidValue @QTypeSystem

-- | for debugging
qUnifyValueToFree ::
       forall t. HasQType 'Negative t
    => QValue
    -> QInterpreter t
qUnifyValueToFree = tsUnifyValueToFree @QTypeSystem
