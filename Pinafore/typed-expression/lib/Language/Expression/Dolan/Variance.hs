module Language.Expression.Dolan.Variance where

import Data.Shim
import Shapes

type DolanVariance = [CCRVariance]

-- How many layers of type abstraction are you on?
type family DolanVarianceKind (dv :: DolanVariance) :: Type where
    DolanVarianceKind '[] = Type
    DolanVarianceKind (sv ': dv) = CCRVarianceKind sv -> DolanVarianceKind dv

type DolanVarianceType = ListType CCRVarianceType

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
        :: forall (sv :: CCRVariance) (dv :: DolanVariance) (f :: CCRVarianceKind sv -> DolanVarianceKind dv).
           HasCCRVariance sv f
        => (forall a. DolanVarianceMap dv (f a))
        -> DolanVarianceMap (sv ': dv) f

dolanVarianceAllInKind :: forall (dv :: DolanVariance). DolanVarianceType dv -> Dict (AllInKind (DolanVarianceKind dv))
dolanVarianceAllInKind NilListType = Dict
dolanVarianceAllInKind (ConsListType _ dvt) =
    case dolanVarianceAllInKind dvt of
        Dict -> Dict

dolanVarianceInKind ::
       forall (dv :: DolanVariance). DolanVarianceType dv -> forall (f :: DolanVarianceKind dv). Dict (InKind f)
dolanVarianceInKind dvt =
    case dolanVarianceAllInKind dvt of
        Dict -> allInKind

bijectSingleVarianceMap ::
       forall (pshim :: PolyShimKind) sv f.
       CCRVarianceType sv
    -> CCRVarianceMap pshim sv f
    -> CCRVarianceMap (PolyIso pshim) sv f
bijectSingleVarianceMap CoCCRVarianceType svm (MkPolyMapT (MkIsomorphism ab ba)) =
    MkPolyMapT $ MkIsomorphism (svm ab) (svm ba)
bijectSingleVarianceMap ContraCCRVarianceType svm (MkCatDual (MkPolyMapT (MkIsomorphism ab ba))) =
    MkPolyMapT $ MkIsomorphism (svm $ MkCatDual ab) (svm $ MkCatDual ba)
bijectSingleVarianceMap RangeCCRVarianceType svm (MkCatRange (MkPolyMapT (MkIsomorphism pab pba)) (MkPolyMapT (MkIsomorphism qab qba))) =
    MkPolyMapT $ MkIsomorphism (svm $ MkCatRange pab qab) (svm $ MkCatRange pba qba)

class Is DolanVarianceType dv => HasDolanVariance (dv :: DolanVariance) (f :: DolanVarianceKind dv) | f -> dv where
    dolanVarianceMap :: DolanVarianceMap dv f

instance HasDolanVariance '[] (f :: Type) where
    dolanVarianceMap = NilDolanVarianceMap

instance (HasCCRVariance sv f, forall a. HasDolanVariance dv (f a), CoercibleKind (DolanVarianceKind dv)) =>
             HasDolanVariance (sv ': dv) (f :: CCRVarianceKind sv -> DolanVarianceKind dv) where
    dolanVarianceMap = ConsDolanVarianceMap dolanVarianceMap
