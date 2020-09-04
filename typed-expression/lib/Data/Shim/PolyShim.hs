module Data.Shim.PolyShim where

import Data.Shim.CatRange
import Data.Shim.JoinMeet
import Data.Shim.Variance
import Shapes

type PolyShimKind = forall k -> ShimKind k

type PolyMorphism :: ShimKind Type -> PolyShimKind
newtype PolyMorphism shim k a b =
    MkPolyMorphism (KindMorphism shim a b)

type PolyFunction :: PolyShimKind
type PolyFunction = PolyMorphism (->)

instance forall (shim :: ShimKind Type) k. InCategory (KindMorphism shim :: ShimKind k) =>
             InCategory (PolyMorphism shim k) where
    cid = MkPolyMorphism cid
    MkPolyMorphism p <.> MkPolyMorphism q = MkPolyMorphism $ p <.> q

instance forall (shim :: ShimKind Type) k. InGroupoid (KindMorphism shim :: ShimKind k) =>
             InGroupoid (PolyMorphism shim k) where
    cinvert (MkPolyMorphism p) = MkPolyMorphism $ cinvert p

instance InCategory (KindMorphism shim :: ShimKind Type) => Category (PolyMorphism shim Type) where
    id = cid
    (.) = (<.>)

instance InGroupoid (KindMorphism shim :: ShimKind Type) => Groupoid (PolyMorphism shim Type) where
    invert = cinvert

type AllInCategory :: PolyShimKind -> Constraint
type AllInCategory pshim = forall k. CoercibleKind k => InCategory (pshim k)

class AllInCategory pshim => ApplyPolyShim (pshim :: PolyShimKind) where
    applyPolyShim ::
           forall k (v :: Variance) (f :: VarianceKind v -> k) (g :: VarianceKind v -> k) (a :: VarianceKind v) (b :: VarianceKind v).
           (InKind a, InKind b, HasVariance v f, HasVariance v g)
        => VarianceType v
        -> pshim (VarianceKind v -> k) f g
        -> VarianceCategory (pshim Type) v a b
        -> pshim k (f a) (g b)

type PEqual :: PolyShimKind
data PEqual k a b where
    MkPEqual :: forall k (a :: k). PEqual k a a

instance forall k. Category (PEqual k) where
    id = MkPEqual
    MkPEqual . MkPEqual = MkPEqual

instance forall k. Groupoid (PEqual k) where
    invert MkPEqual = MkPEqual

instance forall k. InCategory (PEqual k) where
    cid = MkPEqual
    MkPEqual <.> MkPEqual = MkPEqual

instance forall k. InGroupoid (PEqual k) where
    cinvert MkPEqual = MkPEqual

instance ApplyPolyShim PEqual where
    applyPolyShim CovarianceType MkPEqual MkPEqual = MkPEqual
    applyPolyShim ContravarianceType MkPEqual (MkCatDual MkPEqual) = MkPEqual
    applyPolyShim RangevarianceType MkPEqual (MkCatRange MkPEqual MkPEqual) = MkPEqual
