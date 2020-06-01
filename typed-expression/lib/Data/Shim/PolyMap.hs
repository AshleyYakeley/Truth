module Data.Shim.PolyMap where

import Shapes

type MapKind k = k -> k -> Type

type PolyMapKind = forall k -> MapKind k

type PolyMorphism :: MapKind Type -> PolyMapKind
newtype PolyMorphism map k a b =
    MkPolyMorphism (KindMorphism map a b)

type PolyFunction :: PolyMapKind
type PolyFunction = PolyMorphism (->)

instance forall (map :: MapKind Type) k. InCategory (KindMorphism map :: MapKind k) => InCategory (PolyMorphism map k) where
    cid = MkPolyMorphism cid
    MkPolyMorphism p <.> MkPolyMorphism q = MkPolyMorphism $ p <.> q

instance forall (map :: MapKind Type) k. InGroupoid (KindMorphism map :: MapKind k) => InGroupoid (PolyMorphism map k) where
    cinvert (MkPolyMorphism p) = MkPolyMorphism $ cinvert p

instance InCategory (KindMorphism map :: MapKind Type) => Category (PolyMorphism map Type) where
    id = cid
    (.) = (<.>)

instance InGroupoid (KindMorphism map :: MapKind Type) => Groupoid (PolyMorphism map Type) where
    invert = cinvert

type PolyMapT :: (forall k. MapKind k -> MapKind k) -> PolyMapKind -> PolyMapKind
newtype PolyMapT f pmap k a b = MkPolyMapT
    { unPolyMapT :: f (pmap k) a b
    }
