module Data.CatFunctor where

import Control.Category.Dual
import Data.KindMorphism
import Shapes.Import

class CatFunctor (catp :: kp -> kp -> Type) (catq :: kq -> kq -> Type) (f :: kp -> kq) where
    cfmap :: (InKind a, InKind b) => catp a b -> catq (f a) (f b)

instance Functor f => CatFunctor (->) (->) f where
    cfmap = fmap

ccontramap ::
       forall kp kq (catp :: kp -> kp -> Type) (catq :: kq -> kq -> Type) (f :: kp -> kq) a b.
       (CatFunctor (CatDual catp) catq f, InKind a, InKind b)
    => catp a b
    -> catq (f b) (f a)
ccontramap ab = cfmap $ MkCatDual ab

instance Contravariant f => CatFunctor (CatDual (->)) (->) f where
    cfmap (MkCatDual ab) = contramap ab

instance CatFunctor (CatDual (->)) (NestedMorphism (->)) (->) where
    cfmap (MkCatDual ab) = MkNestedMorphism $ \bp -> bp . ab

instance CatFunctor (->) (NestedMorphism (->)) (,) where
    cfmap ab = MkNestedMorphism $ \(a, p) -> (ab a, p)

instance CatFunctor (->) (NestedMorphism (->)) Either where
    cfmap ab =
        MkNestedMorphism $ \case
            Left a -> Left $ ab a
            Right p -> Right p
