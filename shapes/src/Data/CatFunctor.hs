module Data.CatFunctor where

import Shapes.Import

class CatFunctor t f where
    cfmap :: t a b -> t (f a) (f b)

instance (Functor f) => CatFunctor (->) f where
    cfmap = fmap
