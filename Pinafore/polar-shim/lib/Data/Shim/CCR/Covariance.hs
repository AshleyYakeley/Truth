module Data.Shim.CCR.Covariance where

import Shapes

import Data.Shim.CCR.Variance
import Data.Shim.CCR.Variances
import Data.Shim.Mono
import Data.Shim.Poly

type CovaryMap :: forall k. k -> Type
data CovaryMap f where
    NilCovaryMap :: forall (f :: Type). CovaryMap f
    ConsCovaryMap ::
        forall (k :: Type) (f :: Type -> k).
        CCRVariation CoCCRVariance f ->
        (forall (a :: Type). CovaryMap (f a)) ->
        CovaryMap f

type HasCovaryMap :: forall k. k -> Constraint
class HasCovaryMap f where
    covarymap :: CovaryMap f

instance HasCovaryMap (f :: Type) where
    covarymap = NilCovaryMap

instance
    forall k (f :: Type -> k).
    (HasVariance f, VarianceOf f ~ 'Covariance, forall a. HasCovaryMap (f a)) =>
    HasCovaryMap f
    where
    covarymap = ConsCovaryMap ccrVariation covarymap

type CovaryType = ListType ((:~:) CoCCRVariance)

type family CovaryKindCCRVariances (k :: Type) :: CCRVariances where
    CovaryKindCCRVariances Type = '[]
    CovaryKindCCRVariances (Type -> k) = CoCCRVariance ': (CovaryKindCCRVariances k)

covaryToCCRVariancesType :: CovaryType dv -> CCRVariancesType dv
covaryToCCRVariancesType = mapListType (\Refl -> CoCCRVarianceType)

ccrVariancesToCovaryType :: CCRVariancesType dv -> Maybe (CovaryType dv)
ccrVariancesToCovaryType =
    mapMListType $ \case
        CoCCRVarianceType -> Just Refl
        _ -> Nothing

covaryToCCRVariancesMap ::
    forall (dv :: CCRVariances) (f :: CCRVariancesKind dv). CovaryType dv -> CovaryMap f -> CCRVariancesMap dv f
covaryToCCRVariancesMap NilListType NilCovaryMap = NilCCRVariancesMap
covaryToCCRVariancesMap (ConsListType Refl ml) (ConsCovaryMap ccrv vr) =
    ConsCCRVariancesMap ccrv $ covaryToCCRVariancesMap ml vr

ccrVariancesMapToCovary ::
    forall (dv :: CCRVariances) (f :: CCRVariancesKind dv). CovaryType dv -> CCRVariancesMap dv f -> CovaryMap f
ccrVariancesMapToCovary NilListType NilCCRVariancesMap = NilCovaryMap
ccrVariancesMapToCovary (ConsListType Refl ml) (ConsCCRVariancesMap ccrv vr) =
    ConsCovaryMap ccrv $ ccrVariancesMapToCovary ml vr

covaryCoercibleKind :: forall dv. CovaryType dv -> Dict (CoercibleKind (CCRVariancesKind dv))
covaryCoercibleKind NilListType = Dict
covaryCoercibleKind (ConsListType Refl ml) =
    case covaryCoercibleKind ml of
        Dict -> Dict

covaryKMCategory ::
    forall (pmap :: PolyShimKind) dv.
    CCRVariancesPolyShim pmap =>
    CovaryType dv ->
    Dict (CoercibleKind (CCRVariancesKind dv), Category (pmap (CCRVariancesKind dv)))
covaryKMCategory lc = ccrVariancesCategory @pmap $ covaryToCCRVariancesType lc
