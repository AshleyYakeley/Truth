module Data.IsoVariant where

import Shapes.Import

class IsoVariant f where
    isoMap :: (a -> b) -> (b -> a) -> f a -> f b
    default isoMap :: Functor f => (a -> b) -> (b -> a) -> f a -> f b
    isoMap ab _ = fmap ab

class IsoVariant' f where
    isoMap' :: (a -> b) -> (b -> a) -> f a t -> f b t

instance IsoVariant []

instance IsoVariant Maybe

instance IsoVariant Identity

instance IsoVariant ((->) a)

instance IsoVariant' (->) where
    isoMap' _ ba at b = at $ ba b

instance IsoVariant ((,) a)

instance IsoVariant' (,) where
    isoMap' ab _ (a, t) = (ab a, t)

instance IsoVariant (Either a)

instance IsoVariant' Either where
    isoMap' ab _ (Left a) = Left $ ab a
    isoMap' _ _ (Right t) = Right t

enumMap :: (IsoVariant f, Enum a) => f Int -> f a
enumMap = isoMap toEnum fromEnum

infixr 3 <***>, ***>, <***

class IsoVariant f => Productish f where
    pUnit :: f ()
    (<***>) :: f a -> f b -> f (a, b)
    (***>) :: f () -> f a -> f a
    fu ***> fa = isoMap (\((), a) -> a) (\a -> ((), a)) $ fu <***> fa
    (<***) :: f a -> f () -> f a
    fa <*** fu = isoMap (\(a, ()) -> a) (\a -> (a, ())) $ fa <***> fu

infixr 2 <+++>

class IsoVariant f => Summish f where
    pNone :: f None
    (<+++>) :: f a -> f b -> f (Either a b)
