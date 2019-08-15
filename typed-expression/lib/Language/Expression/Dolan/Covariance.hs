module Language.Expression.Dolan.Covariance where

import Data.Shim
import Language.Expression.Dolan.Variance
import Shapes

data CovaryMap (cat :: forall kc. kc -> kc -> Type) (f :: k) where
    NilCovaryMap :: forall (cat :: forall kc. kc -> kc -> Type) (f :: Type). CovaryMap cat f
    ConsCovaryMap
        :: forall (cat :: forall kc. kc -> kc -> Type) (k :: Type) (f :: Type -> k). HasVariance 'Covariance f
        => (forall (a :: Type). CovaryMap cat (f a))
        -> CovaryMap cat f

covaryMapInKind :: forall (cat :: forall kc. kc -> kc -> Type) k (f :: k). CovaryMap cat f -> Dict (InKind f)
covaryMapInKind NilCovaryMap = Dict
covaryMapInKind (ConsCovaryMap cvm) =
    case covaryMapInKind @cat cvm of
        Dict -> Dict

bijectCovaryMap :: forall k (f :: k). CovaryMap JMShim f -> CovaryMap JMIsoShim f
bijectCovaryMap NilCovaryMap = NilCovaryMap
bijectCovaryMap (ConsCovaryMap cv) = ConsCovaryMap $ bijectCovaryMap cv

class HasCovaryMap (f :: k) where
    covarymap :: CovaryMap JMShim f

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
       forall (cat :: forall kc. kc -> kc -> Type) (dv :: DolanVariance) (f :: DolanVarianceKind dv).
       CovaryType dv
    -> CovaryMap cat f
    -> DolanVarianceMap cat dv f
covaryToDolanVarianceMap NilListType NilCovaryMap = NilDolanVarianceMap
covaryToDolanVarianceMap (ConsListType Refl ml) (ConsCovaryMap vr) =
    ConsDolanVarianceMap $ covaryToDolanVarianceMap ml vr

covaryKMCategory ::
       forall (cat :: forall kc. kc -> kc -> Type) dv. DolanVarianceInCategory cat
    => CovaryType dv
    -> Dict ( CoercibleKind (DolanVarianceKind dv)
            , InCategory (cat :: DolanVarianceKind dv -> DolanVarianceKind dv -> Type))
covaryKMCategory lc = dolanVarianceInCategory @cat (mapListType (\Refl -> CovarianceType) lc)
