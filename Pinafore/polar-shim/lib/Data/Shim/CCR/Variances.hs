module Data.Shim.CCR.Variances where

import Shapes

import Data.Shim.CCR.Apply
import Data.Shim.CCR.Variance
import Data.Shim.Poly

type CCRVariances = [CCRVariance]

type family CCRVariancesKind (dv :: CCRVariances) :: Type where
    CCRVariancesKind '[] = Type
    CCRVariancesKind (sv ': dv) = CCRVarianceKind sv -> CCRVariancesKind dv

type CCRVariancesType = ListType CCRVarianceType

class ApplyPolyShim pshim => CCRVariancesPolyShim (pshim :: PolyShimKind) where
    ccrVariancesCategory ::
        forall dv.
        CCRVariancesType dv ->
        Dict (CoercibleKind (CCRVariancesKind dv), Category (pshim (CCRVariancesKind dv)))

instance CCRVariancesPolyShim NullPolyShim where
    ccrVariancesCategory NilListType = Dict
    ccrVariancesCategory (ConsListType _ lt) =
        case ccrVariancesCategory @NullPolyShim lt of
            Dict -> Dict

instance CCRVariancesPolyShim IdentityPolyShim where
    ccrVariancesCategory NilListType = Dict
    ccrVariancesCategory (ConsListType _ lt) =
        case ccrVariancesCategory @IdentityPolyShim lt of
            Dict -> Dict

instance forall (pshim :: PolyShimKind). CCRVariancesPolyShim pshim => CCRVariancesPolyShim (DualPolyT pshim) where
    ccrVariancesCategory NilListType = Dict
    ccrVariancesCategory (ConsListType _ lt) =
        case ccrVariancesCategory @pshim lt of
            Dict -> Dict

instance forall (pshim :: PolyShimKind). CCRVariancesPolyShim pshim => CCRVariancesPolyShim (IsoPolyT pshim) where
    ccrVariancesCategory NilListType = Dict
    ccrVariancesCategory (ConsListType _ lt) =
        case ccrVariancesCategory @pshim lt of
            Dict -> Dict

instance
    forall (pshim :: PolyShimKind) m.
    (CCRVariancesPolyShim pshim, Applicative m) =>
    CCRVariancesPolyShim (ComposePolyT m pshim)
    where
    ccrVariancesCategory NilListType = Dict
    ccrVariancesCategory (ConsListType _ lt) =
        case ccrVariancesCategory @pshim lt of
            Dict -> Dict

ccrVariancesHasKM :: forall dv. CCRVariancesType dv -> Dict (HasKindMorphism (CCRVariancesKind dv))
ccrVariancesHasKM NilListType = Dict
ccrVariancesHasKM (ConsListType _ lt) =
    case ccrVariancesHasKM lt of
        Dict -> Dict

type CCRVariancesMap :: forall (dv :: CCRVariances) -> CCRVariancesKind dv -> Type
data CCRVariancesMap dv f where
    NilCCRVariancesMap :: forall (f :: Type). CCRVariancesMap '[] f
    ConsCCRVariancesMap ::
        forall (sv :: CCRVariance) (dv :: CCRVariances) (f :: CCRVarianceKind sv -> CCRVariancesKind dv).
        CCRVariation sv f ->
        (forall a. CCRVariancesMap dv (f a)) ->
        CCRVariancesMap (sv ': dv) f

nextCCRVariancesMap ::
    forall (sv :: CCRVariance) (dv :: CCRVariances) (f :: CCRVarianceKind sv -> CCRVariancesKind dv).
    CCRVariancesMap (sv ': dv) f ->
    forall a.
    CCRVariancesMap dv (f a)
nextCCRVariancesMap (ConsCCRVariancesMap _ ccrm) = ccrm

lazyDVKindMorphism ::
    forall dv (a :: CCRVariancesKind dv) (b :: CCRVariancesKind dv).
    CCRVariancesType dv ->
    KindMorphism (->) a b ->
    KindMorphism (->) a b
lazyDVKindMorphism NilListType ab = ab
lazyDVKindMorphism (ConsListType _ dvt) ~(MkNestedMorphism ab) = MkNestedMorphism $ lazyDVKindMorphism dvt ab

lazyCCRVariation ::
    forall (sv :: CCRVariance) dv (f :: CCRVarianceKind sv -> CCRVariancesKind dv).
    CCRVarianceType sv ->
    CCRVariancesType dv ->
    CCRVariation sv f ->
    CCRVariation sv f
lazyCCRVariation _ dvt ~(MkCCRVariation _mr mp) = MkCCRVariation Nothing $ \ab -> lazyDVKindMorphism dvt $ mp ab

lazyCCRVariancesMap :: CCRVariancesType dv -> CCRVariancesMap dv t -> CCRVariancesMap dv t
lazyCCRVariancesMap NilListType _cdvm = NilCCRVariancesMap
lazyCCRVariancesMap (ConsListType svt dvt) cdvm =
    ConsCCRVariancesMap
        ( lazyCCRVariation svt dvt
            $ case cdvm of
                ConsCCRVariancesMap ccrv _ -> ccrv
        )
        $ lazyCCRVariancesMap
            dvt
            ( case cdvm of
                ConsCCRVariancesMap _ dv -> dv
            )

class Is CCRVariancesType dv => HasCCRVariances (dv :: CCRVariances) (f :: CCRVariancesKind dv) | f -> dv where
    ccrVariancesMap :: CCRVariancesMap dv f

instance HasCCRVariances '[] (f :: Type) where
    ccrVariancesMap = NilCCRVariancesMap

instance
    (HasCCRVariance sv f, forall a. HasCCRVariances dv (f a)) =>
    HasCCRVariances (sv ': dv) (f :: CCRVarianceKind sv -> CCRVariancesKind dv)
    where
    ccrVariancesMap = ConsCCRVariancesMap ccrVariation ccrVariancesMap
