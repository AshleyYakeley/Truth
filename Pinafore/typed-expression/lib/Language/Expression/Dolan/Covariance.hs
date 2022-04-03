module Language.Expression.Dolan.Covariance where

import Data.Shim
import Language.Expression.Dolan.Variance
import Shapes

type CovaryMap :: forall k. k -> Type
data CovaryMap f where
    NilCovaryMap :: forall (f :: Type). CovaryMap f
    ConsCovaryMap
        :: forall (k :: Type) (f :: Type -> k).
           CCRVariation CoCCRVariance f
        -> (forall (a :: Type). CovaryMap (f a))
        -> CovaryMap f

type HasCovaryMap :: forall k. k -> Constraint
class HasCovaryMap f where
    covarymap :: CovaryMap f

instance HasCovaryMap (f :: Type) where
    covarymap = NilCovaryMap

instance forall k (f :: Type -> k). (HasVariance f, VarianceOf f ~ 'Covariance, forall a. HasCovaryMap (f a)) =>
             HasCovaryMap f where
    covarymap = ConsCovaryMap ccrVariation covarymap

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
covaryToDolanVarianceMap (ConsListType Refl ml) (ConsCovaryMap ccrv vr) =
    ConsDolanVarianceMap ccrv $ covaryToDolanVarianceMap ml vr

dolanVarianceMapToCovary ::
       forall (dv :: DolanVariance) (f :: DolanVarianceKind dv). CovaryType dv -> DolanVarianceMap dv f -> CovaryMap f
dolanVarianceMapToCovary NilListType NilDolanVarianceMap = NilCovaryMap
dolanVarianceMapToCovary (ConsListType Refl ml) (ConsDolanVarianceMap ccrv vr) =
    ConsCovaryMap ccrv $ dolanVarianceMapToCovary ml vr

covaryCoercibleKind :: forall dv. CovaryType dv -> Dict (CoercibleKind (DolanVarianceKind dv))
covaryCoercibleKind NilListType = Dict
covaryCoercibleKind (ConsListType Refl ml) =
    case covaryCoercibleKind ml of
        Dict -> Dict

covaryKMCategory ::
       forall (pmap :: PolyShimKind) dv. DolanVarianceCategory pmap
    => CovaryType dv
    -> Dict (CoercibleKind (DolanVarianceKind dv), Category (pmap (DolanVarianceKind dv)))
covaryKMCategory lc = dolanVarianceCategory @pmap $ covaryToDolanVarianceType lc
