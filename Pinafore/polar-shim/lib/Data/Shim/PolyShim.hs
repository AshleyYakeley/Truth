module Data.Shim.PolyShim where

import Data.Shim.CCRVariance
import Data.Shim.CatRange
import Data.Shim.JoinMeet
import Shapes

type PolyShimKind = forall k -> ShimKind k

type PolyMorphism :: ShimKind Type -> PolyShimKind
newtype PolyMorphism shim k a b =
    MkPolyMorphism (KindMorphism shim a b)

type PolyFunction :: PolyShimKind
type PolyFunction = PolyMorphism (->)

instance forall (shim :: ShimKind Type) k. Category (KindMorphism shim :: ShimKind k) => Category (PolyMorphism shim k) where
    id = MkPolyMorphism id
    MkPolyMorphism p . MkPolyMorphism q = MkPolyMorphism $ p . q

instance forall (shim :: ShimKind Type) k. Groupoid (KindMorphism shim :: ShimKind k) => Groupoid (PolyMorphism shim k) where
    invert (MkPolyMorphism p) = MkPolyMorphism $ invert p

type AllCategory :: PolyShimKind -> Constraint
type AllCategory pshim = forall k. CoercibleKind k => Category (pshim k)

type ApplyPolyShim :: PolyShimKind -> Constraint
class AllCategory pshim => ApplyPolyShim pshim where
    applyPolyShim ::
           forall k (v :: CCRVariance) (f :: CCRVarianceKind v -> k) (g :: CCRVarianceKind v -> k) (a :: CCRVarianceKind v) (b :: CCRVarianceKind v).
           CCRVarianceType v
        -> CCRVariation v f
        -> CCRVariation v g
        -> pshim (CCRVarianceKind v -> k) f g
        -> CCRVarianceCategory (pshim Type) v a b
        -> pshim k (f a) (g b)

applyCoPolyShim ::
       forall (pshim :: PolyShimKind) k (f :: Type -> k) (g :: Type -> k) (a :: Type) (b :: Type). ApplyPolyShim pshim
    => CCRVariation CoCCRVariance f
    -> CCRVariation CoCCRVariance g
    -> pshim (Type -> k) f g
    -> pshim Type a b
    -> pshim k (f a) (g b)
applyCoPolyShim ccrvf ccrvg fg ab = applyPolyShim CoCCRVarianceType ccrvf ccrvg fg ab

applyContraPolyShim ::
       forall (pshim :: PolyShimKind) k (f :: Type -> k) (g :: Type -> k) (a :: Type) (b :: Type). ApplyPolyShim pshim
    => CCRVariation ContraCCRVariance f
    -> CCRVariation ContraCCRVariance g
    -> pshim (Type -> k) f g
    -> pshim Type b a
    -> pshim k (f a) (g b)
applyContraPolyShim ccrvf ccrvg fg ba = applyPolyShim ContraCCRVarianceType ccrvf ccrvg fg (MkCatDual ba)

applyRangePolyShim ::
       forall (pshim :: PolyShimKind) k (f :: (Type, Type) -> k) (g :: (Type, Type) -> k) (a1 :: Type) (a2 :: Type) (b1 :: Type) (b2 :: Type).
       ApplyPolyShim pshim
    => CCRVariation 'RangeCCRVariance f
    -> CCRVariation 'RangeCCRVariance g
    -> pshim ((Type, Type) -> k) f g
    -> pshim Type b1 a1
    -> pshim Type a2 b2
    -> pshim k (f '( a1, a2)) (g '( b1, b2))
applyRangePolyShim ccrvf ccrvg fg ba1 ab2 = applyPolyShim RangeCCRVarianceType ccrvf ccrvg fg (MkCatRange ba1 ab2)

type PEqual :: PolyShimKind
data PEqual k a b where
    MkPEqual :: forall k (a :: k). PEqual k a a

instance forall k. Category (PEqual k) where
    id = MkPEqual
    MkPEqual . MkPEqual = MkPEqual

instance forall k. Groupoid (PEqual k) where
    invert MkPEqual = MkPEqual

instance ApplyPolyShim PEqual where
    applyPolyShim CoCCRVarianceType _ _ MkPEqual MkPEqual = MkPEqual
    applyPolyShim ContraCCRVarianceType _ _ MkPEqual (MkCatDual MkPEqual) = MkPEqual
    applyPolyShim RangeCCRVarianceType _ _ MkPEqual (MkCatRange MkPEqual MkPEqual :: _ a b) = MkPEqual

-- | used for dealing with laziness for recursivly-constructed shims
type ReduciblePolyShim :: PolyShimKind -> Constraint
class AllCategory pshim => ReduciblePolyShim pshim where
    type ReducedPolyShim pshim :: PolyShimKind
    reduceShim ::
           forall a b c d.
           (ReducedPolyShim pshim Type a b -> ReducedPolyShim pshim Type c d)
        -> pshim Type a b
        -> pshim Type c d
    -- type ReducedPolyShim (pshim :: PolyShimKind) = pshim
    default reduceShim ::
        forall a b c d.
            (ReducedPolyShim pshim Type ~ pshim Type) =>
                    (ReducedPolyShim pshim Type a b -> ReducedPolyShim pshim Type c d) -> pshim Type a b -> pshim Type c d
    reduceShim f = f

instance ReduciblePolyShim PEqual where
    type ReducedPolyShim PEqual = PEqual
