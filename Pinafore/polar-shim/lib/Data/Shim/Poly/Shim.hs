module Data.Shim.Poly.Shim where

import Shapes

import Data.Shim.Mono

type PolyShimKind = forall k -> ShimKind k

type PolyMorphism :: ShimKind Type -> PolyShimKind
newtype PolyMorphism shim k a b
    = MkPolyMorphism (KindMorphism shim a b)

type PolyFunction :: PolyShimKind
type PolyFunction = PolyMorphism (->)

instance forall (shim :: ShimKind Type) k. Category (KindMorphism shim :: ShimKind k) => Category (PolyMorphism shim k) where
    id = MkPolyMorphism id
    MkPolyMorphism p . MkPolyMorphism q = MkPolyMorphism $ p . q

instance forall (shim :: ShimKind Type) k. Groupoid (KindMorphism shim :: ShimKind k) => Groupoid (PolyMorphism shim k) where
    invert (MkPolyMorphism p) = MkPolyMorphism $ invert p

type AllCategory :: PolyShimKind -> Constraint
type AllCategory pshim = forall k. CoercibleKind k => Category (pshim k)

-- | used for dealing with laziness for recursivly-constructed shims
type ReduciblePolyShim :: PolyShimKind -> Constraint
class AllCategory pshim => ReduciblePolyShim pshim where
    type ReducedPolyShim pshim :: PolyShimKind
    reduceShim ::
        forall a b c d.
        (ReducedPolyShim pshim Type a b -> ReducedPolyShim pshim Type c d) ->
        pshim Type a b ->
        pshim Type c d
    -- type ReducedPolyShim (pshim :: PolyShimKind) = pshim
    default reduceShim ::
        forall a b c d.
        ReducedPolyShim pshim Type ~ pshim Type =>
        (ReducedPolyShim pshim Type a b -> ReducedPolyShim pshim Type c d) -> pshim Type a b -> pshim Type c d
    reduceShim f = f
