module Data.CatFunctor where

import Control.Category.Dual
import Data.KindMorphism
import Shapes.Import

class CatFunctor (catp :: kp -> kp -> Type) (catq :: kq -> kq -> Type) (f :: kp -> kq) where
    cfmap :: forall (a :: kp) (b :: kp). catp a b -> catq (f a) (f b)

instance Functor f => CatFunctor (->) (->) f where
    cfmap = fmap

instance CatFunctor catp catq f => CatFunctor (CatDual catp) (CatDual catq) f where
    cfmap (MkCatDual ab) = MkCatDual $ cfmap ab

ccontramap ::
       forall kp kq (catp :: kp -> kp -> Type) (catq :: kq -> kq -> Type) (f :: kp -> kq) (a :: kp) (b :: kp).
       (CatFunctor (CatDual catp) catq f)
    => catp a b
    -> catq (f b) (f a)
ccontramap ab = cfmap $ MkCatDual ab

endocfmap ::
       forall k (cat :: k -> k -> Type) (f :: k -> k) (a :: k) (b :: k). (CatFunctor cat cat f)
    => cat a b
    -> cat (f a) (f b)
endocfmap = cfmap

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

cfmap1 ::
       forall catp (catq :: Type -> Type -> Type) f a b p. (CatFunctor catp (NestedMorphism catq) f)
    => catp a b
    -> catq (f a p) (f b p)
cfmap1 c = unNestedMorphism $ cfmap c

cfmap2 ::
       forall catp (catq :: Type -> Type -> Type) f a b p q. (CatFunctor catp (NestedMorphism catq) f)
    => catp a b
    -> catq (f a p q) (f b p q)
cfmap2 c = unNestedMorphism $ unNestedMorphism $ cfmap c

cfmap3 ::
       forall catp (catq :: Type -> Type -> Type) f a b p q r. (CatFunctor catp (NestedMorphism catq) f)
    => catp a b
    -> catq (f a p q r) (f b p q r)
cfmap3 c = unNestedMorphism $ unNestedMorphism $ unNestedMorphism $ cfmap c

cfmap4 ::
       forall catp (catq :: Type -> Type -> Type) f a b p q r s. (CatFunctor catp (NestedMorphism catq) f)
    => catp a b
    -> catq (f a p q r s) (f b p q r s)
cfmap4 c = unNestedMorphism $ unNestedMorphism $ unNestedMorphism $ unNestedMorphism $ cfmap c

ccontramap1 ::
       forall catp (catq :: Type -> Type -> Type) f a b p. (CatFunctor (CatDual catp) (NestedMorphism catq) f)
    => catp b a
    -> catq (f a p) (f b p)
ccontramap1 c = cfmap1 $ MkCatDual c
