module Data.Shim.CCR.Nonpolar where

import Shapes

import Data.Shim.CCR.Argument
import Data.Shim.CCR.Arguments
import Data.Shim.CCR.Variance
import Data.Shim.CCR.Variances

type NonpolarArgument :: (Type -> Type) -> CCRArgumentKind
data NonpolarArgument w sv t where
    CoNonpolarArgument ::
        forall w t. w t -> NonpolarArgument w CoCCRVariance t
    ContraNonpolarArgument ::
        forall w t. w t -> NonpolarArgument w ContraCCRVariance t
    RangeNonpolarArgument ::
        forall w p q.
        w p ->
        w q ->
        NonpolarArgument w 'RangeCCRVariance '(p, q)

instance forall (w :: Type -> Type). TestEquality w => IsCCRArg (NonpolarArgument w) where
    ccrArgumentType (CoNonpolarArgument _) = CoCCRVarianceType
    ccrArgumentType (ContraNonpolarArgument _) = ContraCCRVarianceType
    ccrArgumentType (RangeNonpolarArgument _ _) = RangeCCRVarianceType
    ccrArgumentTestEquality (CoNonpolarArgument a) (CoNonpolarArgument b) = do
        Refl <- testEquality a b
        return Refl
    ccrArgumentTestEquality (ContraNonpolarArgument a) (ContraNonpolarArgument b) = do
        Refl <- testEquality a b
        return Refl
    ccrArgumentTestEquality (RangeNonpolarArgument ap aq) (RangeNonpolarArgument bp bq) = do
        Refl <- testEquality ap bp
        Refl <- testEquality aq bq
        return Refl

type NonpolarArguments :: (Type -> Type) -> forall (dv :: CCRVariances) -> CCRVariancesKind dv -> Type -> Type
type NonpolarArguments w = CCRArguments (NonpolarArgument w)

mapNonpolarArgument ::
    forall (w :: Type -> Type) sv.
    (w --> w) -> NonpolarArgument w sv --> NonpolarArgument w sv
mapNonpolarArgument f (CoNonpolarArgument t) = CoNonpolarArgument $ f t
mapNonpolarArgument f (ContraNonpolarArgument t) = ContraNonpolarArgument $ f t
mapNonpolarArgument f (RangeNonpolarArgument tp tq) = RangeNonpolarArgument (f tp) (f tq)

mapNonpolarArguments ::
    forall (w :: Type -> Type) dv gt.
    (w --> w) -> NonpolarArguments w dv gt --> NonpolarArguments w dv gt
mapNonpolarArguments _ NilCCRArguments = NilCCRArguments
mapNonpolarArguments f (ConsCCRArguments arg args) = ConsCCRArguments (mapNonpolarArgument f arg) (mapNonpolarArguments f args)
