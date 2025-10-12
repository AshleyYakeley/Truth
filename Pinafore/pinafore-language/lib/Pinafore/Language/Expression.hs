{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Expression where

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Convert.Types ()
import Pinafore.Language.Error
import Pinafore.Language.Interpreter
import Pinafore.Language.Type
import Pinafore.Language.VarID

qTypeError :: TypeError QGroundType -> QError
qTypeError = \case
    PatternTypeError paterr -> PatternErrorError paterr
    ExpressionTypeError (UndefinedBindingsError ww) -> nameWitnessErrorType ww
    InternalTypeError msg -> InternalError Nothing $ toNamedText msg
    InternalSafetyTypeError msg rterr t ->
        InternalError Nothing
            $ toNamedText msg
            <> " simplification: "
            <> showNamedText rterr
            <> " recursive type: "
            <> showNamedText t
    UninvertibleTypeError t -> TypeNotInvertibleError $ exprShow t
    NoGroundConvertTypeError ga gb -> NoGroundTypeConversionError (showGroundType ga) (showGroundType gb)
    IncoherentGroundConvertTypeError ga gb -> IncoherentGroundTypeConversionError (showGroundType ga) (showGroundType gb)
    ConvertTypeError fta ftb ->
        flipToType fta $ \(ta :: _ pola _) ->
            flipToType ftb $ \(tb :: _ polb _) ->
                TypeConvertError
                    (exprShow ta)
                    (witnessToValue $ polarityType @pola)
                    (exprShow tb)
                    (witnessToValue $ polarityType @polb)

qRunTypeResult :: QTypeResult --> QInterpreter
qRunTypeResult = \case
    SuccessResult a -> return a
    FailureResult err -> throw $ qTypeError err

qGetRunTypeM :: QInterpreter (WRaised QTypeM QTypeResult)
qGetRunTypeM = do
    entries <- getSubtypeConversions
    return $ MkWRaised $ runDolanTypeMEntries entries

qRunTypeM :: QTypeM --> QInterpreter
qRunTypeM ma = do
    entries <- getSubtypeConversions
    qRunTypeResult $ runDolanTypeMEntries entries ma

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
qSubstitute f expr = qRunTypeM $ tsSubstitute @QTypeSystem f expr

qImply :: [(ImplicitName, QExpression)] -> QExpression -> QInterpreter QExpression
qImply substs expr = let
    substf (ImplicitVarID vn) = lookup vn substs
    substf _ = Nothing
    in qSubstitute substf expr

qAbstractExpr :: VarID -> QExpression -> QInterpreter QExpression
qAbstractExpr var expr = qRunTypeM $ tsAbstract @QTypeSystem var expr

qAbstractsExpr :: [VarID] -> QExpression -> QInterpreter QExpression
qAbstractsExpr [] e = return e
qAbstractsExpr (n : nn) e = do
    e' <- qAbstractsExpr nn e
    qAbstractExpr n e'

qPolyAbstractF ::
    forall p q. VarID -> QShimWit 'Positive p -> QFExpression ((->) q) -> QInterpreter (QFExpression ((->) (p, q)))
qPolyAbstractF var tw expr = qRunTypeM $ tsPolyAbstractF @QTypeSystem var tw expr

qSimplify ::
    forall a.
    TSMappable QTypeSystem a =>
    a ->
    QInterpreter a
qSimplify val = qRunTypeM $ tsSimplify @QTypeSystem val

qVarPattern :: VarID -> QPattern
qVarPattern = tsVarPattern @QTypeSystem

qAnyPattern :: QPattern
qAnyPattern = tsAnyPattern @QTypeSystem

qBothPattern :: QPattern -> QPattern -> QInterpreter QPattern
qBothPattern pat1 pat2 = qRunTypeM $ tsBothPattern @QTypeSystem pat1 pat2

qToPatternConstructor ::
    forall t lt.
    (ToListShimWit QShim (QType 'Positive) lt, FromPolarShimWit QShim (QType 'Negative) t) =>
    QPurityFunction t lt ->
    QPatternConstructor
qToPatternConstructor tml = toPatternConstructor (fromPolarShimWit @Type @QShim @(QType 'Negative)) toListShimWit tml

qApplyPatternConstructor :: QPatternConstructor -> QPattern -> QInterpreter (QPatternConstructor)
qApplyPatternConstructor qcons pat = qRunTypeM $ tsApplyPatternConstructor @QTypeSystem qcons pat

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
qPartialExpressionSumList exprs = qRunTypeM $ tsPartialExpressionSumList @QTypeSystem exprs

qLambdaPatternMatch :: VarID -> QPattern -> QMatch
qLambdaPatternMatch = tsLambdaPatternMatch @QTypeSystem

qExpressionPatternMatch :: QExpression -> QPattern -> QInterpreter QMatch
qExpressionPatternMatch expr pat = qRunTypeM $ tsExpressionPatternMatch @QTypeSystem expr pat

qMatchGate :: QMatch -> QPartialExpression -> QInterpreter QPartialExpression
qMatchGate match expr = qRunTypeM $ tsMatchGate @QTypeSystem match expr

qMatchBindings :: String -> QMatch -> [(VarID, QExpression)]
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
qApplyExpr exprf expra = qRunTypeM $ tsApply @QTypeSystem exprf expra

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

qSubsume :: QShimWit 'Positive inf -> QType 'Positive decl -> QInterpreter (QOpenExpression (QShim inf decl))
qSubsume winf tdecl = qRunTypeM $ tsSubsume @QTypeSystem winf tdecl

qSubsumeExpr :: Some (QType 'Positive) -> QExpression -> QInterpreter QExpression
qSubsumeExpr tw expr = qRunTypeM $ tsSubsumeExpression @QTypeSystem tw expr

qSubsumeFExpr :: Functor f => Some (QType 'Positive) -> QFExpression f -> QInterpreter (QFExpression f)
qSubsumeFExpr tw expr = qRunTypeM $ tsSubsumeFExpression @QTypeSystem tw expr

qLetExpr :: VarID -> QExpression -> QExpression -> QInterpreter QExpression
qLetExpr name exp body = qRunTypeM $ tsLet @QTypeSystem name exp body

qUncheckedBindingsRecursiveLetExpr :: [QBinding] -> QInterpreter (Map VarID (DefDoc, QExpression))
qUncheckedBindingsRecursiveLetExpr binds = qRunTypeM $ tsUncheckedRecursiveLet @QTypeSystem binds

qBindingSequentialLetExpr :: QBinding -> QInterpreter (Map VarID (DefDoc, QExpression))
qBindingSequentialLetExpr bind = qRunTypeM $ tsSequentialLet @QTypeSystem bind

qEvalExpr ::
    QExpression ->
    QInterpreter QValue
qEvalExpr expr = qRunTypeResult $ tsEval @QTypeSystem expr

qEvalExprMaybe :: QExpression -> Maybe QValue
qEvalExprMaybe expr = tsEvalMaybe @QTypeSystem expr

qUnifyRigid :: forall a b. QShimWit 'Positive a -> QShimWit 'Negative b -> QInterpreter (QShim a b)
qUnifyRigid tw vw = qRunTypeM $ tsUnifyRigid @QTypeSystem tw vw

qUnifyValueTo :: forall t. QShimWit 'Negative t -> QValue -> QInterpreter t
qUnifyValueTo vw val = qRunTypeM $ tsUnifyValueTo @QTypeSystem vw val

qValue ::
    forall t.
    HasQType QPolyShim 'Positive t =>
    t ->
    QValue
qValue v = MkSomeOf toPolarShimWit v

qUnifyExpressionToOpen :: forall t. QShimWit 'Negative t -> QExpression -> QInterpreter (QOpenExpression t)
qUnifyExpressionToOpen tw expr = qRunTypeM $ tsUnifyExpressionTo @QTypeSystem tw expr

qSubsumeValue :: forall t. QType 'Positive t -> QValue -> QInterpreter t
qSubsumeValue tw val = qRunTypeM $ tsSubsumeValue @QTypeSystem tw val

qSubsumeExpressionToOpen :: forall t. QType 'Positive t -> QExpression -> QInterpreter (QOpenExpression t)
qSubsumeExpressionToOpen tw expr = qRunTypeM $ tsSubsumeExpressionTo @QTypeSystem mempty tw expr

qSubsumeExpressionToOpenWit :: forall t. QIsoShimWit 'Positive t -> QExpression -> QInterpreter (QOpenExpression t)
qSubsumeExpressionToOpenWit (MkShimWit t iconv) expr = do
    oexpr <- qSubsumeExpressionToOpen t expr
    return $ fmap (shimToFunction $ polarPolyIsoNegative iconv) oexpr

qUnifyValue ::
    forall t.
    HasQType QPolyShim 'Negative t =>
    QValue ->
    QInterpreter t
qUnifyValue val = qRunTypeM $ tsUnifyValue @QTypeSystem val

qExactValue :: QType 'Positive t -> QValue -> Maybe t
qExactValue wt (MkSomeOf (MkShimWit wt' (MkPolarShim conv)) v) = do
    Refl <- testEquality wt wt'
    return $ shimToFunction conv v

qUnifyF :: forall f. (forall t. QShimWit 'Negative t -> QShimWit 'Negative (f t)) -> QValue -> QInterpreter (QValueF f)
qUnifyF f val = qRunTypeM $ tsUnifyF @QTypeSystem f val

-- | for debugging
qUnifyRigidValue ::
    forall t.
    HasQType QPolyShim 'Negative t =>
    QValue ->
    QInterpreter t
qUnifyRigidValue val = qRunTypeM $ tsUnifyRigidValue @QTypeSystem val

-- | for debugging
qUnifyValueToFree ::
    forall t.
    HasQType QPolyShim 'Negative t =>
    QValue ->
    QInterpreter t
qUnifyValueToFree val = qRunTypeM $ tsUnifyValueToFree @QTypeSystem val
