module Language.Expression.Dolan.Covariance where

import Data.Shim
import Language.Expression.Dolan.Variance
import Shapes

type CovaryMap :: forall k. k -> Type
data CovaryMap f where
    NilCovaryMap :: forall (f :: Type). CovaryMap f
    ConsCovaryMap
        :: forall (k :: Type) (f :: Type -> k). HasCCRVariance CoCCRVariance f
        => (forall (a :: Type). CovaryMap (f a))
        -> CovaryMap f

covaryMapInKind :: forall k (f :: k). CovaryMap f -> Dict (InKind f)
covaryMapInKind NilCovaryMap = Dict
covaryMapInKind (ConsCovaryMap cvm) =
    case covaryMapInKind cvm of
        Dict -> Dict

type HasCovaryMap :: forall k. k -> Constraint
class HasCovaryMap f where
    covarymap :: CovaryMap f

instance HasCovaryMap (f :: Type) where
    covarymap = NilCovaryMap

instance HasCovaryMap Maybe where
    covarymap = ConsCovaryMap NilCovaryMap

instance HasCovaryMap [] where
    covarymap = ConsCovaryMap NilCovaryMap

instance HasCovaryMap (,) where
    covarymap = ConsCovaryMap $ ConsCovaryMap NilCovaryMap

instance HasCovaryMap Either where
    covarymap = ConsCovaryMap $ ConsCovaryMap NilCovaryMap

type CovaryType = ListType ((:~:) CoCCRVariance)

type family CovaryKindDolanVariance (k :: Type) :: DolanVariance where
    CovaryKindDolanVariance Type = '[]
    CovaryKindDolanVariance (Type -> k) = CoCCRVariance ': (CovaryKindDolanVariance k)

covaryToDolanVarianceType :: CovaryType dv -> DolanVarianceType dv
covaryToDolanVarianceType = mapListType (\Refl -> CoCCRVarianceType)

dolanVarianceToCovaryType :: DolanVarianceType dv -> Maybe (CovaryType dv)
dolanVarianceToCovaryType =
    mapMListType $ \case
        CoCCRVarianceType -> Just Refl
        _ -> Nothing

covaryToDolanVarianceMap ::
       forall (dv :: DolanVariance) (f :: DolanVarianceKind dv). CovaryType dv -> CovaryMap f -> DolanVarianceMap dv f
covaryToDolanVarianceMap NilListType NilCovaryMap = NilDolanVarianceMap
covaryToDolanVarianceMap (ConsListType Refl ml) (ConsCovaryMap vr) =
    ConsDolanVarianceMap ccrVariation $ covaryToDolanVarianceMap ml vr

covaryCoercibleKind :: forall dv. CovaryType dv -> Dict (CoercibleKind (DolanVarianceKind dv))
covaryCoercibleKind NilListType = Dict
covaryCoercibleKind (ConsListType Refl ml) =
    case covaryCoercibleKind ml of
        Dict -> Dict

covaryKMCategory ::
       forall (pmap :: PolyShimKind) dv. DolanVarianceInCategory pmap
    => CovaryType dv
    -> Dict (CoercibleKind (DolanVarianceKind dv), InCategory (pmap (DolanVarianceKind dv)))
covaryKMCategory lc = dolanVarianceInCategory @pmap $ covaryToDolanVarianceType lc
