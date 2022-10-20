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

qConstExprAny :: QValue -> QExpression
qConstExprAny = tsConst @QTypeSystem

qConstExpr ::
       forall a. HasQType 'Positive a
    => a
    -> QExpression
qConstExpr a = qConstExprAny $ jmToValue a

qVarExpr :: VarID -> QExpression
qVarExpr name = tsVar @QTypeSystem name

qName :: ReferenceName -> QInterpreter QExpression
qName name = do
    mexpr <- lookupLetBinding name
    case mexpr of
        Just (Right expr) -> return expr
        Just (Left v) -> return $ qVarExpr v
        Nothing -> do
            spos <- paramAsk sourcePosParam
            return $ qVarExpr $ mkBadVarID spos name

qAbstractExpr :: VarID -> QExpression -> QInterpreter QExpression
qAbstractExpr name expr = tsAbstract @QTypeSystem name expr

qAbstractsExpr :: [VarID] -> QExpression -> QInterpreter QExpression
qAbstractsExpr [] e = return e
qAbstractsExpr (n:nn) e = do
    e' <- qAbstractsExpr nn e
    qAbstractExpr n e'

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
       forall m. MonadThrow ExpressionError m
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

qCase :: QExpression -> [(QPattern, QExpression)] -> QInterpreter QExpression
qCase = tsCase @QTypeSystem

qFunctionPosWitness :: forall a b. QShimWit 'Negative a -> QShimWit 'Positive b -> QShimWit 'Positive (a -> b)
qFunctionPosWitness = tsFunctionPosShimWit @QTypeSystem

qFunctionPosWitnesses ::
       ListType (QShimWit 'Negative) a -> QShimWit 'Positive b -> QShimWit 'Positive (ListProduct a -> b)
qFunctionPosWitnesses NilListType tb = mapPosShimWit (functionToShim "poswitness" $ \ub -> ub ()) tb
qFunctionPosWitnesses (ConsListType ta la) tb =
    mapPosShimWit (functionToShim "poswitness" $ \f a l -> f (a, l)) $
    qFunctionPosWitness ta $ qFunctionPosWitnesses la tb

qCaseAbstract :: [(QPattern, QExpression)] -> QInterpreter QExpression
qCaseAbstract = tsCaseAbstract @QTypeSystem

qMultiCaseAbstract :: PeanoNatType n -> [(FixedList n QPattern, QExpression)] -> QInterpreter QExpression
qMultiCaseAbstract = tsMultiCaseAbstract @QTypeSystem

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

qBindExpr :: VarID -> Markdown -> Maybe (Some (QType 'Positive)) -> QExpression -> QBinding
qBindExpr = tsSingleBinding @QTypeSystem

qSubsumeExpr :: Some (QType 'Positive) -> QExpression -> QInterpreter QExpression
qSubsumeExpr = tsSubsumeExpression @QTypeSystem

qLetExpr :: VarID -> QExpression -> QExpression -> QInterpreter QExpression
qLetExpr name exp body = tsLet @QTypeSystem name exp body

qUncheckedBindingsRecursiveLetExpr :: [QBinding] -> QInterpreter (Map VarID (Markdown, QExpression))
qUncheckedBindingsRecursiveLetExpr = tsUncheckedRecursiveLet @QTypeSystem

qBindingSequentialLetExpr :: QBinding -> QInterpreter (Map VarID (Markdown, QExpression))
qBindingSequentialLetExpr = tsSequentialLet @QTypeSystem

qEvalExpr ::
       forall m. MonadThrow ExpressionError m
    => QExpression
    -> m QValue
qEvalExpr expr = tsEval @QTypeSystem expr

typedAnyToVal :: forall t. QShimWit 'Negative t -> QValue -> QInterpreter t
typedAnyToVal = tsUnifyValueTo @QTypeSystem

typedUnifyExpressionToOpen :: forall t. QShimWit 'Negative t -> QExpression -> QInterpreter (QOpenExpression t)
typedUnifyExpressionToOpen = tsUnifyExpressionTo @QTypeSystem

typedSubsumeExpressionToOpen :: forall t. QType 'Positive t -> QExpression -> QInterpreter (QOpenExpression t)
typedSubsumeExpressionToOpen = tsSubsumeExpressionTo @QTypeSystem

qUnifyValue ::
       forall t. HasQType 'Negative t
    => QValue
    -> QInterpreter t
qUnifyValue = tsUnifyValue @QTypeSystem

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
