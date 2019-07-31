{-# OPTIONS -fno-warn-orphans #-}

module Data.Shim.RangeLift where

import Data.Shim.CatRange
import Data.Shim.JoinMeet
import Data.Shim.PolyShim
import Shapes

class EnhancedPolyShim shim => RangeLiftShim (shim :: forall k. k -> k -> Type) (f :: (k1, k1) -> k2) where
    rangeLiftShim :: forall (a :: (k1, k1)) (b :: (k1, k1)). CatRange shim a b -> shim (f a) (f b)

{-
instance forall k (shim :: forall kc. kc -> kc -> Type) f. RangeLiftShim shim f => CatFunctor (CatRange (shim :: k -> k -> Type)) shim f where
    cfmap = rangeLiftShim
-}
coRangeLiftShim ::
       forall (shim :: forall k. k -> k -> Type) k1 k2 (f :: (k1, k1) -> k2) p q1 q2.
       (EnhancedPolyShim shim, InKind p, InKind q1, InKind q2)
    => RangeLiftShim shim f => shim q1 q2 -> shim (f '( p, q1)) (f '( p, q2))
coRangeLiftShim qq = rangeLiftShim $ coCatRange qq

contraRangeLiftShim ::
       forall (shim :: forall k. k -> k -> Type) k1 k2 (f :: (k1, k1) -> k2) p1 p2 q.
       (EnhancedPolyShim shim, InKind p1, InKind p2, InKind q)
    => RangeLiftShim shim f => shim p2 p1 -> shim (f '( p1, q)) (f '( p2, q))
contraRangeLiftShim pp = rangeLiftShim $ contraCatRange pp

coRangeLift ::
       forall (shim :: forall k. k -> k -> Type) (f :: (Type, Type) -> Type) p q1 q2. EnhancedPolyShim shim
    => RangeLiftShim shim f => (q1 -> q2) -> f '( p, q1) -> f '( p, q2)
coRangeLift f = fromEnhanced $ coRangeLiftShim @shim $ toEnhanced "coRangeLift" f

contraRangeLift ::
       forall (shim :: forall k. k -> k -> Type) (f :: (Type, Type) -> Type) p1 p2 q. EnhancedPolyShim shim
    => RangeLiftShim shim f => (p2 -> p1) -> f '( p1, q) -> f '( p2, q)
contraRangeLift f = fromEnhanced $ contraRangeLiftShim @shim $ toEnhanced "contraRangeLift" f
