module Language.Expression.Dolan.Variance where

import Data.Shim
import Shapes

type DolanVariance = [Variance]

-- How many layers of type abstraction are you on?
type family DolanVarianceKind (dv :: DolanVariance) :: Type where
    DolanVarianceKind '[] = Type
    DolanVarianceKind (sv ': dv) = VarianceKind sv -> DolanVarianceKind dv

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
data DolanVarianceMap dv f where
    NilDolanVarianceMap :: forall (f :: Type). DolanVarianceMap '[] f
    ConsDolanVarianceMap
        :: forall (sv :: Variance) (dv :: DolanVariance) (f :: VarianceKind sv -> DolanVarianceKind dv).
           HasVariance sv f
        => (forall a. DolanVarianceMap dv (f a))
        -> DolanVarianceMap (sv ': dv) f

dolanVarianceMapInKind ::
       forall (dv :: DolanVariance) (f :: DolanVarianceKind dv). DolanVarianceMap dv f -> Dict (InKind f)
dolanVarianceMapInKind NilDolanVarianceMap = Dict
dolanVarianceMapInKind (ConsDolanVarianceMap dvm) =
    case dolanVarianceMapInKind dvm of
        Dict -> Dict

bijectSingleVarianceMap ::
       forall (pshim :: PolyShimKind) sv f.
       VarianceType sv
    -> VarianceMap pshim sv f
    -> VarianceMap (PolyIso pshim) sv f
bijectSingleVarianceMap CovarianceType svm (MkPolyMapT (MkIsomorphism ab ba)) =
    MkPolyMapT $ MkIsomorphism (svm ab) (svm ba)
bijectSingleVarianceMap ContravarianceType svm (MkCatDual (MkPolyMapT (MkIsomorphism ab ba))) =
    MkPolyMapT $ MkIsomorphism (svm $ MkCatDual ab) (svm $ MkCatDual ba)
bijectSingleVarianceMap RangevarianceType svm (MkCatRange (MkPolyMapT (MkIsomorphism pab pba)) (MkPolyMapT (MkIsomorphism qab qba))) =
    MkPolyMapT $ MkIsomorphism (svm $ MkCatRange pab qab) (svm $ MkCatRange pba qba)

class Is DolanVarianceType dv => HasDolanVariance (dv :: DolanVariance) (f :: DolanVarianceKind dv) | f -> dv where
    dolanVarianceMap :: DolanVarianceMap dv f

instance HasDolanVariance '[] (f :: Type) where
    dolanVarianceMap = NilDolanVarianceMap

instance (HasVariance sv f, forall a. HasDolanVariance dv (f a), CoercibleKind (DolanVarianceKind dv)) =>
             HasDolanVariance (sv ': dv) (f :: VarianceKind sv -> DolanVarianceKind dv) where
    dolanVarianceMap = ConsDolanVarianceMap dolanVarianceMap
