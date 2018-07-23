module Data.Bijection where

import Data.CatFunctor
import Data.IsoVariant
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

biIsoMap :: IsoVariant f => Bijection a b -> f a -> f b
biIsoMap (MkBijection ab ba) = isoMap ab ba

biIsoBi :: IsoVariant f => Bijection a b -> Bijection (f a) (f b)
biIsoBi (MkBijection ab ba) = MkBijection (isoMap ab ba) (isoMap ba ab)

biIsoMap' :: IsoVariant' f => Bijection a b -> f a t -> f b t
biIsoMap' (MkBijection ab ba) = isoMap' ab ba

biIsoBi' :: IsoVariant' f => Bijection a b -> Bijection (f a t) (f b t)
biIsoBi' (MkBijection ab ba) = MkBijection (isoMap' ab ba) (isoMap' ba ab)

biFBi :: Functor f => Bijection a b -> Bijection (f a) (f b)
biFBi (MkBijection ab ba) = MkBijection (fmap ab) (fmap ba)

biSwap :: Bijection (a, b) (b, a)
biSwap = MkBijection swap swap

packBijection :: IsSequence t => Bijection [Element t] t
packBijection = MkBijection pack unpack

unpackBijection :: IsSequence t => Bijection t [Element t]
unpackBijection = MkBijection unpack pack
