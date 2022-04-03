module Pinafore.Language.Grammar.Interpret.TypeDecl.Parameter where

import Pinafore.Language.Type
import Shapes

type CCRTypeParam :: CCRArgumentKind
data CCRTypeParam (sv :: CCRVariance) (t :: CCRVarianceKind sv) where
    CoCCRTypeParam :: SymbolType n -> CCRTypeParam CoCCRVariance (UVarT n)
    ContraCCRTypeParam :: SymbolType n -> CCRTypeParam ContraCCRVariance (UVarT n)
    RangeCCRTypeParam :: SymbolType np -> SymbolType nq -> CCRTypeParam 'RangeCCRVariance '( UVarT np, UVarT nq) -- contra, co

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
assignCCRTypeParam (CoCCRTypeParam v) call = assignUVarT @a v call
assignCCRTypeParam (ContraCCRTypeParam v) call = assignUVarT @a v call
assignCCRTypeParam (RangeCCRTypeParam vp vq) call =
    case unsafeTypeIsPair @_ @_ @a of
        Refl -> assignUVarT @(Contra a) vp $ assignUVarT @(Co a) vq call

tParamVars :: CCRTypeParam sv t -> [AnyW SymbolType]
tParamVars (CoCCRTypeParam t) = [MkAnyW t]
tParamVars (ContraCCRTypeParam t) = [MkAnyW t]
tParamVars (RangeCCRTypeParam p q) = [MkAnyW p, MkAnyW q]

type CCRTypeParams :: forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type -> Type
type CCRTypeParams = CCRArguments CCRTypeParam
