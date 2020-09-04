module Language.Expression.Dolan.Variance where

import Data.Shim
import Shapes

type DolanVariance = [Variance]

-- How many layers of type abstraction are you on?
type family DolanVarianceKind (dv :: DolanVariance) :: Type where
    DolanVarianceKind '[] = Type
    DolanVarianceKind (v ': dv) = VarianceKind v -> DolanVarianceKind dv

type DolanVarianceType = ListType VarianceType

class ApplyPolyShim pshim => DolanVarianceInCategory (pshim :: PolyShimKind) where
    dolanVarianceInCategory ::
           forall dv.
           DolanVarianceType dv
        -> Dict (CoercibleKind (DolanVarianceKind dv), InCategory (pshim (DolanVarianceKind dv)))

instance DolanVarianceInCategory PEqual where
    dolanVarianceInCategory NilListType = Dict
    dolanVarianceInCategory (ConsListType _ lt) =
        case dolanVarianceInCategory @PEqual lt of
            Dict -> Dict

instance DolanVarianceInCategory JMShim where
    dolanVarianceInCategory NilListType = Dict
    dolanVarianceInCategory (ConsListType _ lt) =
        case dolanVarianceInCategory @JMShim lt of
            Dict -> Dict

instance forall (pshim :: PolyShimKind). (DolanVarianceInCategory pshim) => DolanVarianceInCategory (PolyIso pshim) where
    dolanVarianceInCategory NilListType = Dict
    dolanVarianceInCategory (ConsListType _ lt) =
        case dolanVarianceInCategory @pshim lt of
            Dict -> Dict

instance forall (pshim :: PolyShimKind) m. (DolanVarianceInCategory pshim, Applicative m) =>
             DolanVarianceInCategory (PolyComposeShim m pshim) where
    dolanVarianceInCategory NilListType = Dict
    dolanVarianceInCategory (ConsListType _ lt) =
        case dolanVarianceInCategory @pshim lt of
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
       forall (pshim :: PolyShimKind) sv gt.
       VarianceType sv
    -> VarianceMap pshim sv gt
    -> VarianceMap (PolyIso pshim) sv gt
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
