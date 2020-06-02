module Language.Expression.Dolan.Variance where

import Data.Shim
import Shapes

type DolanVariance = [Variance]

-- How many layers of type abstraction are you on?
type family DolanVarianceKind (dv :: DolanVariance) :: Type where
    DolanVarianceKind '[] = Type
    DolanVarianceKind (v ': dv) = VarianceKind v -> DolanVarianceKind dv

type DolanVarianceType = ListType VarianceType

class ApplyPolyShim pmap => DolanVarianceInCategory (pmap :: PolyMapKind) where
    dolanVarianceInCategory ::
           forall dv.
           DolanVarianceType dv
        -> Dict (CoercibleKind (DolanVarianceKind dv), InCategory (pmap (DolanVarianceKind dv)))

instance DolanVarianceInCategory JMShim where
    dolanVarianceInCategory NilListType = Dict
    dolanVarianceInCategory (ConsListType _ lt) =
        case dolanVarianceInCategory @JMShim lt of
            Dict -> Dict

instance forall (pmap :: PolyMapKind). (DolanVarianceInCategory pmap) => DolanVarianceInCategory (PolyIso pmap) where
    dolanVarianceInCategory NilListType = Dict
    dolanVarianceInCategory (ConsListType _ lt) =
        case dolanVarianceInCategory @pmap lt of
            Dict -> Dict

dolanVarianceHasKM :: forall dv. DolanVarianceType dv -> Dict (HasKindMorphism (DolanVarianceKind dv))
dolanVarianceHasKM NilListType = Dict
dolanVarianceHasKM (ConsListType _ lt) =
    case dolanVarianceHasKM lt of
        Dict -> Dict

type DolanVarianceMap :: forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type
data DolanVarianceMap dv gt where
    NilDolanVarianceMap :: forall (gt :: Type). DolanVarianceMap '[] gt
    ConsDolanVarianceMap
        :: forall (sv :: Variance) (dv :: DolanVariance) (gt :: VarianceKind sv -> DolanVarianceKind dv).
           HasVariance sv gt
        => (forall a. DolanVarianceMap dv (gt a))
        -> DolanVarianceMap (sv ': dv) gt

dolanVarianceMapInKind ::
       forall (dv :: DolanVariance) (gt :: DolanVarianceKind dv). DolanVarianceMap dv gt -> Dict (InKind gt)
dolanVarianceMapInKind NilDolanVarianceMap = Dict
dolanVarianceMapInKind (ConsDolanVarianceMap dvm) =
    case dolanVarianceMapInKind dvm of
        Dict -> Dict

bijectSingleVarianceMap ::
       forall (pmap :: PolyMapKind) sv gt. VarianceType sv -> VarianceMap pmap sv gt -> VarianceMap (PolyIso pmap) sv gt
bijectSingleVarianceMap CovarianceType svm (MkPolyMapT (MkIsomorphism ab ba)) =
    MkPolyMapT $ MkIsomorphism (svm ab) (svm ba)
bijectSingleVarianceMap ContravarianceType svm (MkCatDual (MkPolyMapT (MkIsomorphism ab ba))) =
    MkPolyMapT $ MkIsomorphism (svm $ MkCatDual ab) (svm $ MkCatDual ba)
bijectSingleVarianceMap RangevarianceType svm (MkCatRange (MkPolyMapT (MkIsomorphism pab pba)) (MkPolyMapT (MkIsomorphism qab qba))) =
    MkPolyMapT $ MkIsomorphism (svm $ MkCatRange pab qab) (svm $ MkCatRange pba qba)

class HasDolanVary (dv :: DolanVariance) (f :: DolanVarianceKind dv) | f -> dv where
    dolanVary :: DolanVarianceMap dv f

instance HasDolanVary '[] (f :: Type) where
    dolanVary = NilDolanVarianceMap

instance (HasVariance v f, forall a. HasDolanVary vv (f a), CoercibleKind (DolanVarianceKind vv)) =>
             HasDolanVary (v ': vv) (f :: VarianceKind v -> DolanVarianceKind vv) where
    dolanVary = ConsDolanVarianceMap dolanVary
