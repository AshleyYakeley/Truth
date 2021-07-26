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

type ApplyPolyShim :: PolyShimKind -> Constraint
class AllInCategory pshim => ApplyPolyShim pshim where
    applyPolyShim ::
           forall k (v :: Variance) (f :: VarianceKind v -> k) (g :: VarianceKind v -> k) (a :: VarianceKind v) (b :: VarianceKind v).
           (InKind a, InKind b, HasVariance v f, HasVariance v g)
        => VarianceType v
        -> pshim (VarianceKind v -> k) f g
        -> VarianceCategory (pshim Type) v a b
        -> pshim k (f a) (g b)

applyCoPolyShim ::
       forall (pshim :: PolyShimKind) k (f :: Type -> k) (g :: Type -> k) (a :: Type) (b :: Type).
       (ApplyPolyShim pshim, HasVariance 'Covariance f, HasVariance 'Covariance g)
    => pshim (Type -> k) f g
    -> pshim Type a b
    -> pshim k (f a) (g b)
applyCoPolyShim fg ab = applyPolyShim CovarianceType fg ab

applyContraPolyShim ::
       forall (pshim :: PolyShimKind) k (f :: Type -> k) (g :: Type -> k) (a :: Type) (b :: Type).
       (ApplyPolyShim pshim, HasVariance 'Contravariance f, HasVariance 'Contravariance g)
    => pshim (Type -> k) f g
    -> pshim Type b a
    -> pshim k (f a) (g b)
applyContraPolyShim fg ba = applyPolyShim ContravarianceType fg (MkCatDual ba)

applyRangePolyShim ::
       forall (pshim :: PolyShimKind) k (f :: (Type, Type) -> k) (g :: (Type, Type) -> k) (a1 :: Type) (a2 :: Type) (b1 :: Type) (b2 :: Type).
       (ApplyPolyShim pshim, HasVariance 'Rangevariance f, HasVariance 'Rangevariance g)
    => pshim ((Type, Type) -> k) f g
    -> pshim Type b1 a1
    -> pshim Type a2 b2
    -> pshim k (f '( a1, a2)) (g '( b1, b2))
applyRangePolyShim fg ba1 ab2 = applyPolyShim RangevarianceType fg (MkCatRange ba1 ab2)

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

-- | used for dealing with laziness for recursivly-constructed shims
type ReduciblePolyShim :: PolyShimKind -> Constraint
class AllInCategory pshim => ReduciblePolyShim pshim where
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
