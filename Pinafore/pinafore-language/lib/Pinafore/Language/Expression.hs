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

qConstExprAny :: PinaforeValue -> PinaforeExpression
qConstExprAny = tsConst @PinaforeTypeSystem

qConstExpr ::
       forall a. HasPinaforeType 'Positive a
    => a
    -> PinaforeExpression
qConstExpr a = qConstExprAny $ jmToValue a

qVarExpr :: VarID -> PinaforeExpression
qVarExpr name = tsVar @PinaforeTypeSystem name

qName :: ReferenceName -> PinaforeInterpreter PinaforeExpression
qName name = do
    mexpr <- lookupLetBinding name
    case mexpr of
        Just (Right expr) -> return expr
        Just (Left v) -> return $ qVarExpr v
        Nothing -> do
            spos <- paramAsk sourcePosParam
            return $ qVarExpr $ mkBadVarID spos name

qAbstractExpr :: VarID -> PinaforeExpression -> PinaforeInterpreter PinaforeExpression
qAbstractExpr name expr = tsAbstract @PinaforeTypeSystem name expr

qAbstractsExpr :: [VarID] -> PinaforeExpression -> PinaforeInterpreter PinaforeExpression
qAbstractsExpr [] e = return e
qAbstractsExpr (n:nn) e = do
    e' <- qAbstractsExpr nn e
    qAbstractExpr n e'

qVarPattern :: VarID -> PinaforePattern
qVarPattern = tsVarPattern @PinaforeTypeSystem

qAnyPattern :: PinaforePattern
qAnyPattern = tsAnyPattern @PinaforeTypeSystem

qBothPattern :: PinaforePattern -> PinaforePattern -> PinaforeInterpreter PinaforePattern
qBothPattern = tsBothPattern @PinaforeTypeSystem

qToPatternConstructor ::
       forall t lt.
       ( ToListShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) lt
       , FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) t
       )
    => PurityFunction Maybe t (ListProduct lt)
    -> PinaforePatternConstructor
qToPatternConstructor tml =
    toExpressionPatternConstructor $
    toPatternConstructor (fromPolarShimWit @Type @(PinaforePolyShim Type) @(PinaforeType 'Negative)) toListShimWit tml

qApplyPatternConstructor ::
       PinaforePatternConstructor -> PinaforePattern -> PinaforeInterpreter (PinaforePatternConstructor)
qApplyPatternConstructor = tsApplyPatternConstructor @PinaforeTypeSystem

qSealPatternConstructor ::
       forall m. MonadThrow ExpressionError m
    => PinaforePatternConstructor
    -> m PinaforePattern
qSealPatternConstructor = tsSealPatternConstructor @PinaforeTypeSystem

qApplyAllPatternConstructor ::
       PinaforePatternConstructor -> [PinaforePattern] -> PinaforeInterpreter (PinaforePatternConstructor)
qApplyAllPatternConstructor pc [] = return pc
qApplyAllPatternConstructor pc (pat:pats) = do
    pc' <- qApplyPatternConstructor pc pat
    qApplyAllPatternConstructor pc' pats

qConstructPattern :: PinaforePatternConstructor -> [PinaforePattern] -> PinaforeInterpreter PinaforePattern
qConstructPattern pc pats = do
    pc' <- qApplyAllPatternConstructor pc pats
    qSealPatternConstructor pc'

qCase :: PinaforeExpression -> [(PinaforePattern, PinaforeExpression)] -> PinaforeInterpreter PinaforeExpression
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

qCaseAbstract :: [(PinaforePattern, PinaforeExpression)] -> PinaforeInterpreter PinaforeExpression
qCaseAbstract = tsCaseAbstract @PinaforeTypeSystem

qMultiCaseAbstract ::
       PeanoNatType n -> [(FixedList n PinaforePattern, PinaforeExpression)] -> PinaforeInterpreter PinaforeExpression
qMultiCaseAbstract = tsMultiCaseAbstract @PinaforeTypeSystem

qApplyExpr :: PinaforeExpression -> PinaforeExpression -> PinaforeInterpreter PinaforeExpression
qApplyExpr exprf expra = tsApply @PinaforeTypeSystem exprf expra

qApplyAllExpr :: PinaforeExpression -> [PinaforeExpression] -> PinaforeInterpreter PinaforeExpression
qApplyAllExpr e [] = return e
qApplyAllExpr e (a:aa) = do
    e' <- qApplyExpr e a
    qApplyAllExpr e' aa

qEmptyList :: PinaforeExpression
qEmptyList = qConstExpr $ [] @BottomType

qConsList :: PinaforeExpression
qConsList = qConstExpr $ (:|) @A

qSequenceExpr :: [PinaforeExpression] -> PinaforeInterpreter PinaforeExpression
qSequenceExpr [] = return $ qEmptyList
qSequenceExpr (e:ee) = do
    ee' <- qSequenceExpr ee
    qApplyAllExpr qConsList [e, ee']

qBindExpr :: VarID -> Markdown -> Maybe (Some (PinaforeType 'Positive)) -> PinaforeExpression -> PinaforeBinding
qBindExpr = tsSingleBinding @PinaforeTypeSystem

qSubsumeExpr :: Some (PinaforeType 'Positive) -> PinaforeExpression -> PinaforeInterpreter PinaforeExpression
qSubsumeExpr = tsSubsumeExpression @PinaforeTypeSystem

qLetExpr :: VarID -> PinaforeExpression -> PinaforeExpression -> PinaforeInterpreter PinaforeExpression
qLetExpr name exp body = tsLet @PinaforeTypeSystem name exp body

qUncheckedBindingsRecursiveLetExpr ::
       [PinaforeBinding] -> PinaforeInterpreter (Map VarID (Markdown, PinaforeExpression))
qUncheckedBindingsRecursiveLetExpr = tsUncheckedRecursiveLet @PinaforeTypeSystem

qBindingSequentialLetExpr :: PinaforeBinding -> PinaforeInterpreter (Map VarID (Markdown, PinaforeExpression))
qBindingSequentialLetExpr = tsSequentialLet @PinaforeTypeSystem

qEvalExpr ::
       forall m. MonadThrow ExpressionError m
    => PinaforeExpression
    -> m PinaforeValue
qEvalExpr expr = tsEval @PinaforeTypeSystem expr

typedAnyToVal :: forall t. PinaforeShimWit 'Negative t -> PinaforeValue -> PinaforeInterpreter t
typedAnyToVal = tsUnifyValueTo @PinaforeTypeSystem

typedUnifyExpressionToOpen ::
       forall t. PinaforeShimWit 'Negative t -> PinaforeExpression -> PinaforeInterpreter (PinaforeOpenExpression t)
typedUnifyExpressionToOpen = tsUnifyExpressionTo @PinaforeTypeSystem

typedSubsumeExpressionToOpen ::
       forall t. PinaforeType 'Positive t -> PinaforeExpression -> PinaforeInterpreter (PinaforeOpenExpression t)
typedSubsumeExpressionToOpen = tsSubsumeExpressionTo @PinaforeTypeSystem

typedAnyToPinaforeVal ::
       forall t. HasPinaforeType 'Negative t
    => PinaforeValue
    -> PinaforeInterpreter t
typedAnyToPinaforeVal = tsUnifyValue @PinaforeTypeSystem

-- | for debugging
rigidTypedAnyToPinaforeVal ::
       forall t. HasPinaforeType 'Negative t
    => PinaforeValue
    -> PinaforeInterpreter t
rigidTypedAnyToPinaforeVal = tsUnifyRigidValue @PinaforeTypeSystem
