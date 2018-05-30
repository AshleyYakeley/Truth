module Data.Bijection where

import Data.CatFunctor
import Shapes.Import

data Bijection a b = MkBijection
    { biForwards :: a -> b
    , biBackwards :: b -> a
    }
    -- biForwards . biBackwards = id
    -- biBackwards . biForwards = id

instance Category Bijection where
    id = MkBijection id id
    (MkBijection p1 q1) . (MkBijection p2 q2) = MkBijection (p1 . p2) (q2 . q1)

instance (Functor f) => CatFunctor Bijection f where
    cfmap bi = MkBijection {biForwards = fmap (biForwards bi), biBackwards = fmap (biBackwards bi)}

invertBijection :: Bijection a b -> Bijection b a
invertBijection (MkBijection ab ba) = MkBijection ba ab

mapBijectionIn :: Bijection a1 a2 -> Bijection a1 b -> Bijection a2 b
mapBijectionIn (MkBijection a1a2 a2a1) (MkBijection a1b ba1) = MkBijection (a1b . a2a1) (a1a2 . ba1)

mapBijectionOut :: Bijection b1 b2 -> Bijection a b1 -> Bijection a b2
mapBijectionOut (MkBijection b1b2 b2b1) (MkBijection ab1 b1a) = MkBijection (b1b2 . ab1) (b1a . b2b1)

funcBijection :: Bijection p1 p2 -> Bijection q1 q2 -> Bijection (p1 -> q1) (p2 -> q2)
funcBijection (MkBijection p1p2 p2p1) (MkBijection q1q2 q2q1) =
    MkBijection (\p1q1 -> q1q2 . p1q1 . p2p1) (\p2q2 -> q2q1 . p2q2 . p1p2)

biSwap :: Bijection (a, b) (b, a)
biSwap = MkBijection swap swap

packBijection :: IsSequence t => Bijection [Element t] t
packBijection = MkBijection pack unpack

unpackBijection :: IsSequence t => Bijection t [Element t]
unpackBijection = MkBijection unpack pack
