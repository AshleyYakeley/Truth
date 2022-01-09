module Language.Expression.Dolan.Variance where

import Data.Shim
import Shapes

type DolanVariance = [CCRVariance]

type family DolanVarianceKind (dv :: DolanVariance) :: Type where
    DolanVarianceKind '[] = Type
    DolanVarianceKind (sv ': dv) = CCRVarianceKind sv -> DolanVarianceKind dv

type DolanVarianceType = ListType CCRVarianceType

class ApplyPolyShim pshim => DolanVarianceCategory (pshim :: PolyShimKind) where
    dolanVarianceCategory ::
           forall dv.
           DolanVarianceType dv
        -> Dict (CoercibleKind (DolanVarianceKind dv), Category (pshim (DolanVarianceKind dv)))

instance DolanVarianceCategory PEqual where
    dolanVarianceCategory NilListType = Dict
    dolanVarianceCategory (ConsListType _ lt) =
        case dolanVarianceCategory @PEqual lt of
            Dict -> Dict

instance DolanVarianceCategory JMShim where
    dolanVarianceCategory NilListType = Dict
    dolanVarianceCategory (ConsListType _ lt) =
        case dolanVarianceCategory @JMShim lt of
            Dict -> Dict

instance forall (pshim :: PolyShimKind). (DolanVarianceCategory pshim) => DolanVarianceCategory (PolyIso pshim) where
    dolanVarianceCategory NilListType = Dict
    dolanVarianceCategory (ConsListType _ lt) =
        case dolanVarianceCategory @pshim lt of
            Dict -> Dict

instance forall (pshim :: PolyShimKind) m. (DolanVarianceCategory pshim, Applicative m) =>
             DolanVarianceCategory (PolyComposeShim m pshim) where
    dolanVarianceCategory NilListType = Dict
    dolanVarianceCategory (ConsListType _ lt) =
        case dolanVarianceCategory @pshim lt of
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
           CCRVariation sv f
        -> (forall a. DolanVarianceMap dv (f a))
        -> DolanVarianceMap (sv ': dv) f

lazyDVKindMorphism ::
       forall dv (a :: DolanVarianceKind dv) (b :: DolanVarianceKind dv).
       DolanVarianceType dv
    -> KindMorphism (->) a b
    -> KindMorphism (->) a b
lazyDVKindMorphism NilListType ab = ab
lazyDVKindMorphism (ConsListType _ dvt) ~(MkNestedMorphism ab) = MkNestedMorphism $ lazyDVKindMorphism dvt ab

lazyCCRVariation ::
       forall (sv :: CCRVariance) dv (f :: CCRVarianceKind sv -> DolanVarianceKind dv).
       CCRVarianceType sv
    -> DolanVarianceType dv
    -> CCRVariation sv f
    -> CCRVariation sv f
lazyCCRVariation _ dvt ~(MkCCRVariation _mr mp) = MkCCRVariation Nothing $ \ab -> lazyDVKindMorphism dvt $ mp ab

lazyDolanVarianceMap :: DolanVarianceType dv -> DolanVarianceMap dv t -> DolanVarianceMap dv t
lazyDolanVarianceMap NilListType _cdvm = NilDolanVarianceMap
lazyDolanVarianceMap (ConsListType svt dvt) cdvm =
    ConsDolanVarianceMap
        (lazyCCRVariation svt dvt $
         case cdvm of
             ConsDolanVarianceMap ccrv _ -> ccrv) $
    lazyDolanVarianceMap
        dvt
        (case cdvm of
             ConsDolanVarianceMap _ dv -> dv)

class Is DolanVarianceType dv => HasDolanVariance (dv :: DolanVariance) (f :: DolanVarianceKind dv) | f -> dv where
    dolanVarianceMap :: DolanVarianceMap dv f

instance HasDolanVariance '[] (f :: Type) where
    dolanVarianceMap = NilDolanVarianceMap

instance (HasCCRVariance sv f, forall a. HasDolanVariance dv (f a), CoercibleKind (DolanVarianceKind dv)) =>
             HasDolanVariance (sv ': dv) (f :: CCRVarianceKind sv -> DolanVarianceKind dv) where
    dolanVarianceMap = ConsDolanVarianceMap ccrVariation dolanVarianceMap
