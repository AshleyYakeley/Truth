module Pinafore.Language.Interpret.TypeDecl.Parameter where

import Import

type CCRTypeParam :: CCRArgumentKind
data CCRTypeParam (sv :: CCRVariance) (t :: CCRVarianceKind sv) where
    CoCCRTypeParam :: TypeVarT tv -> CCRTypeParam CoCCRVariance tv
    ContraCCRTypeParam :: TypeVarT tv -> CCRTypeParam ContraCCRVariance tv
    RangeCCRTypeParam :: TypeVarT tvp -> TypeVarT tvq -> CCRTypeParam 'RangeCCRVariance '( tvp, tvq) -- contra, co

instance IsCCRArg CCRTypeParam where
    ccrArgumentType (CoCCRTypeParam _) = CoCCRVarianceType
    ccrArgumentType (ContraCCRTypeParam _) = ContraCCRVarianceType
    ccrArgumentType (RangeCCRTypeParam _ _) = RangeCCRVarianceType
    ccrArgumentTestEquality (CoCCRTypeParam arg1) (CoCCRTypeParam arg2) = do
        Refl <- testEquality arg1 arg2
        return Refl
    ccrArgumentTestEquality (ContraCCRTypeParam arg1) (ContraCCRTypeParam arg2) = do
        Refl <- testEquality arg1 arg2
        return Refl
    ccrArgumentTestEquality (RangeCCRTypeParam p1 q1) (RangeCCRTypeParam p2 q2) = do
        Refl <- testEquality p1 p2
        Refl <- testEquality q1 q2
        return Refl

assignCCRTypeParam ::
       forall (sv :: CCRVariance) (a :: CCRVarianceKind sv) (t :: CCRVarianceKind sv) r.
       CCRTypeParam sv t
    -> (t ~ a => r)
    -> r
assignCCRTypeParam (CoCCRTypeParam v) call = assignTypeVarT @a v call
assignCCRTypeParam (ContraCCRTypeParam v) call = assignTypeVarT @a v call
assignCCRTypeParam (RangeCCRTypeParam vp vq) call =
    case unsafeTypeIsPair @_ @_ @a of
        Refl -> assignTypeVarT @(Contra a) vp $ assignTypeVarT @(Co a) vq call

tParamVars :: CCRTypeParam sv t -> [SomeTypeVarT]
tParamVars (CoCCRTypeParam t) = [MkSomeTypeVarT t]
tParamVars (ContraCCRTypeParam t) = [MkSomeTypeVarT t]
tParamVars (RangeCCRTypeParam p q) = [MkSomeTypeVarT p, MkSomeTypeVarT q]

type CCRTypeParams :: forall (dv :: CCRVariances) -> CCRVariancesKind dv -> Type -> Type
type CCRTypeParams = CCRArguments CCRTypeParam
