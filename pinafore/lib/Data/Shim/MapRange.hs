{-# OPTIONS -fno-warn-orphans #-}

module Data.Shim.MapRange where

import Data.Shim.JoinMeet
import Data.Shim.PolyShim
import Data.Shim.Range
import Shapes

class PolyShim shim => IsoMapRange (shim :: forall k. k -> k -> Type) (f :: (k1, k1) -> k2) where
    isoMapRange :: CatRange (Isomorphism shim) a b -> shim (f a) (f b)
    default isoMapRange :: MapRange shim f => CatRange (Isomorphism shim) a b -> shim (f a) (f b)
    isoMapRange (MkCatRange a b) = mapRange $ MkCatRange (isoForwards a) (isoForwards b)

isoBiRange ::
       forall (shim :: forall k. k -> k -> Type) k1 k2 (f :: (k1, k1) -> k2) a b.
       (IsoMapRange shim f, InKind a, InKind b)
    => CatRange (Isomorphism shim) a b
    -> Isomorphism shim (f a) (f b)
isoBiRange rbij = MkIsomorphism (isoMapRange rbij) (isoMapRange $ cinvert rbij)

class IsoMapRange shim f => MapRange (shim :: forall k. k -> k -> Type) (f :: (k1, k1) -> k2) where
    mapRange :: forall (a :: (k1, k1)) (b :: (k1, k1)). CatRange shim a b -> shim (f a) (f b)

{-
instance forall k (shim :: forall kc. kc -> kc -> Type) f. MapRange shim f => CatFunctor (CatRange (shim :: k -> k -> Type)) shim f where
    cfmap = mapRange
-}
coMapRange ::
       forall (shim :: forall k. k -> k -> Type) k1 k2 (f :: (k1, k1) -> k2) p q1 q2.
       (PolyShim shim, InKind p, InKind q1, InKind q2)
    => MapRange shim f => shim q1 q2 -> shim (f '( p, q1)) (f '( p, q2))
coMapRange qq = mapRange $ coCatRange qq

contraMapRange ::
       forall (shim :: forall k. k -> k -> Type) k1 k2 (f :: (k1, k1) -> k2) p1 p2 q.
       (PolyShim shim, InKind p1, InKind p2, InKind q)
    => MapRange shim f => shim p2 p1 -> shim (f '( p1, q)) (f '( p2, q))
contraMapRange pp = mapRange $ contraCatRange pp

coMapRangeF ::
       forall (shim :: forall k. k -> k -> Type) (f :: (Type, Type) -> Type) p q1 q2. PolyShim shim
    => MapRange shim f => (q1 -> q2) -> f '( p, q1) -> f '( p, q2)
coMapRangeF f = fromEnhanced $ coMapRange @shim $ toEnhanced f

contraMapRangeF ::
       forall (shim :: forall k. k -> k -> Type) (f :: (Type, Type) -> Type) p1 p2 q. PolyShim shim
    => MapRange shim f => (p2 -> p1) -> f '( p1, q) -> f '( p2, q)
contraMapRangeF f = fromEnhanced $ contraMapRange @shim $ toEnhanced f
