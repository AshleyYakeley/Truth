module Language.Expression.Dolan.Variance where

import Data.Shim
import Shapes

type DolanVariance = [Variance]

-- How many layers of type abstraction are you on?
type family DolanVarianceKind (dv :: DolanVariance) :: Type where
    DolanVarianceKind '[] = Type
    DolanVarianceKind (v ': dv) = VarianceKind v -> DolanVarianceKind dv

type DolanVarianceType = ListType VarianceType

class ConPolyShim cat => DolanVarianceInCategory (cat :: forall k. k -> k -> Type) where
    dolanVarianceInCategory ::
           forall dv.
           DolanVarianceType dv
        -> Dict ( CoercibleKind (DolanVarianceKind dv)
                , InCategory (cat :: DolanVarianceKind dv -> DolanVarianceKind dv -> Type))

instance DolanVarianceInCategory JMShim where
    dolanVarianceInCategory NilListType = Dict
    dolanVarianceInCategory (ConsListType _ lt) =
        case dolanVarianceInCategory @JMShim lt of
            Dict -> Dict

instance DolanVarianceInCategory JMIsoShim where
    dolanVarianceInCategory NilListType = Dict
    dolanVarianceInCategory (ConsListType _ lt) =
        case dolanVarianceInCategory @JMShim lt of
            Dict -> Dict

dolanVarianceHasKM :: forall dv. DolanVarianceType dv -> Dict (HasKindMorphism (DolanVarianceKind dv))
dolanVarianceHasKM NilListType = Dict
dolanVarianceHasKM (ConsListType _ lt) =
    case dolanVarianceHasKM lt of
        Dict -> Dict

data DolanVarianceMap (cat :: forall kc. kc -> kc -> Type) (dv :: DolanVariance) (gt :: DolanVarianceKind dv) where
    NilDolanVarianceMap :: forall (cat :: forall kc. kc -> kc -> Type) (gt :: Type). DolanVarianceMap cat '[] gt
    ConsDolanVarianceMap
        :: forall (cat :: forall kc. kc -> kc -> Type) (sv :: Variance) (dv :: DolanVariance) (gt :: VarianceKind sv -> DolanVarianceKind dv).
           HasVariance sv gt
        => (forall a. DolanVarianceMap cat dv (gt a))
        -> DolanVarianceMap cat (sv ': dv) gt

dolanVarianceMapInKind ::
       forall (cat :: forall kc. kc -> kc -> Type) (dv :: DolanVariance) (gt :: DolanVarianceKind dv).
       DolanVarianceMap cat dv gt
    -> Dict (InKind gt)
dolanVarianceMapInKind NilDolanVarianceMap = Dict
dolanVarianceMapInKind (ConsDolanVarianceMap dvm) =
    case dolanVarianceMapInKind @cat dvm of
        Dict -> Dict

bijectSingleVarianceMap :: VarianceType sv -> VarianceMap JMShim sv gt -> VarianceMap JMIsoShim sv gt
bijectSingleVarianceMap CovarianceType svm (MkJMIsoShim (MkIsomorphism ab ba)) =
    MkJMIsoShim $ MkIsomorphism (svm ab) (svm ba)
bijectSingleVarianceMap ContravarianceType svm (MkCatDual (MkJMIsoShim (MkIsomorphism ab ba))) =
    MkJMIsoShim $ MkIsomorphism (svm $ MkCatDual ab) (svm $ MkCatDual ba)
bijectSingleVarianceMap RangevarianceType svm (MkCatRange (MkJMIsoShim (MkIsomorphism pab pba)) (MkJMIsoShim (MkIsomorphism qab qba))) =
    MkJMIsoShim $ MkIsomorphism (svm $ MkCatRange pab qab) (svm $ MkCatRange pba qba)

bijectDolanVarianceMap :: DolanVarianceMap JMShim dv gt -> DolanVarianceMap JMIsoShim dv gt
bijectDolanVarianceMap NilDolanVarianceMap = NilDolanVarianceMap
bijectDolanVarianceMap (ConsDolanVarianceMap dvm) = ConsDolanVarianceMap $ bijectDolanVarianceMap dvm

class HasDolanVary (dv :: DolanVariance) (f :: DolanVarianceKind dv) | f -> dv where
    dolanVary :: DolanVarianceMap JMShim dv f

instance HasDolanVary '[] (f :: Type) where
    dolanVary = NilDolanVarianceMap

instance (HasVariance v f, forall a. HasDolanVary vv (f a), CoercibleKind (DolanVarianceKind vv)) =>
             HasDolanVary (v ': vv) (f :: VarianceKind v -> DolanVarianceKind vv) where
    dolanVary = ConsDolanVarianceMap dolanVary
