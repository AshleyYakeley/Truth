module Language.Expression.Dolan.Covariance where

import Data.Shim
import Language.Expression.Dolan.Variance
import Shapes

type CovaryMap :: forall k. k -> Type
data CovaryMap f where
    NilCovaryMap :: forall (f :: Type). CovaryMap f
    ConsCovaryMap
        :: forall (k :: Type) (f :: Type -> k). HasVariance 'Covariance f
        => (forall (a :: Type). CovaryMap (f a))
        -> CovaryMap f

covaryMapInKind :: forall k (f :: k). CovaryMap f -> Dict (InKind f)
covaryMapInKind NilCovaryMap = Dict
covaryMapInKind (ConsCovaryMap cvm) =
    case covaryMapInKind cvm of
        Dict -> Dict

class HasCovaryMap (f :: k) where
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

type CovaryType = ListType ((:~:) 'Covariance)

type family CovaryKindDolanVariance (k :: Type) :: DolanVariance where
    CovaryKindDolanVariance Type = '[]
    CovaryKindDolanVariance (Type -> k) = 'Covariance ': (CovaryKindDolanVariance k)

covaryToDolanVarianceMap ::
       forall (dv :: DolanVariance) (f :: DolanVarianceKind dv). CovaryType dv -> CovaryMap f -> DolanVarianceMap dv f
covaryToDolanVarianceMap NilListType NilCovaryMap = NilDolanVarianceMap
covaryToDolanVarianceMap (ConsListType Refl ml) (ConsCovaryMap vr) =
    ConsDolanVarianceMap $ covaryToDolanVarianceMap ml vr

covaryKMCategory ::
       forall (pmap :: PolyMapKind) dv. DolanVarianceInCategory pmap
    => CovaryType dv
    -> Dict (CoercibleKind (DolanVarianceKind dv), InCategory (pmap (DolanVarianceKind dv)))
covaryKMCategory lc = dolanVarianceInCategory @pmap (mapListType (\Refl -> CovarianceType) lc)
