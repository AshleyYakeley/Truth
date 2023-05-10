module Data.Isomorphism where

import Control.Category.Dual
import Control.Category.Groupoid
import Data.CatFunctor
import Data.KindMorphism
import Shapes.Import

data Isomorphism (cat :: k -> k -> Type) (a :: k) (b :: k) = MkIsomorphism
    { isoForwards :: cat a b
    , isoBackwards :: cat b a
    }
    -- isoForwards . isoBackwards = id
    -- isoBackwards . isoForwards = id

type Bijection = Isomorphism (->)

instance Category cat => Category (Isomorphism cat) where
    id = MkIsomorphism id id
    (MkIsomorphism p1 q1) . (MkIsomorphism p2 q2) = MkIsomorphism (p1 . p2) (q2 . q1)

instance Category cat => Groupoid (Isomorphism cat) where
    invert (MkIsomorphism ab ba) = MkIsomorphism ba ab

instance CatFunctor catp catq f => CatFunctor (Isomorphism catp) (Isomorphism catq) (f :: Type -> Type) where
    cfmap bi = MkIsomorphism {isoForwards = cfmap (isoForwards bi), isoBackwards = cfmap (isoBackwards bi)}

instance CatFunctor (CatDual catp) catq f =>
             CatFunctor (CatDual (Isomorphism catp)) (Isomorphism catq) (f :: Type -> Type) where
    cfmap (MkCatDual bi) =
        MkIsomorphism {isoForwards = ccontramap (isoForwards bi), isoBackwards = ccontramap (isoBackwards bi)}

isoMapCat ::
       forall k (cat1 :: k -> k -> Type) (cat2 :: k -> k -> Type) (p :: k) (q :: k).
       (forall (a :: k) (b :: k). cat1 a b -> cat2 a b)
    -> Isomorphism cat1 p q
    -> Isomorphism cat2 p q
isoMapCat m (MkIsomorphism f b) = MkIsomorphism (m f) (m b)

biIsoMap :: Invariant f => Bijection a b -> f a -> f b
biIsoMap (MkIsomorphism ab ba) = invmap ab ba

biIsoBi :: Invariant f => Bijection a b -> Bijection (f a) (f b)
biIsoBi (MkIsomorphism ab ba) = MkIsomorphism (invmap ab ba) (invmap ba ab)

biSwap :: Bijection (a, b) (b, a)
biSwap = MkIsomorphism swap swap

packBijection :: IsSequence t => Bijection [Element t] t
packBijection = MkIsomorphism pack unpack

unpackBijection :: IsSequence t => Bijection t [Element t]
unpackBijection = MkIsomorphism unpack pack

strictBytestringBijection :: Bijection LazyByteString StrictByteString
strictBytestringBijection = MkIsomorphism toStrict fromStrict

class HasKindMorphism (k :: Type) where
    kindMorphismMapCat ::
           forall (cat1 :: Type -> Type -> Type) (cat2 :: Type -> Type -> Type). (Category cat1, Category cat2)
        => (forall p q. cat1 p q -> cat2 p q)
        -> forall (a :: k) (b :: k). KindMorphism cat1 a b -> KindMorphism cat2 a b
    mkKindIsomorphism ::
           forall (cat :: Type -> Type -> Type). Category cat
        => forall (a :: k) (b :: k). KindMorphism cat a b -> KindMorphism cat b a -> KindIsomorphism cat a b

type KindIsomorphism (cat :: Type -> Type -> Type) = KindMorphism (Isomorphism cat)

type KindBijection = KindMorphism Bijection

instance HasKindMorphism Type where
    kindMorphismMapCat ab = ab
    mkKindIsomorphism = MkIsomorphism

instance HasKindMorphism kq => HasKindMorphism (kp -> kq) where
    kindMorphismMapCat ab (MkNestedMorphism a) = MkNestedMorphism $ kindMorphismMapCat ab a
    mkKindIsomorphism ::
           forall (cat :: Type -> Type -> Type). Category cat
        => forall (a :: kp -> kq) (b :: kp -> kq).
                   KindMorphism cat a b -> KindMorphism cat b a -> KindIsomorphism cat a b
    mkKindIsomorphism (MkNestedMorphism ab) (MkNestedMorphism ba) = MkNestedMorphism $ mkKindIsomorphism @_ @cat ab ba

instance (HasKindMorphism kp, HasKindMorphism kq) => HasKindMorphism (kp, kq) where
    kindMorphismMapCat ab (MkPairMorphism pa qa) = MkPairMorphism (kindMorphismMapCat ab pa) (kindMorphismMapCat ab qa)
    mkKindIsomorphism ::
           forall cat (a :: (kp, kq)) b. Category cat
        => KindMorphism cat a b
        -> KindMorphism cat b a
        -> KindIsomorphism cat a b
    mkKindIsomorphism (MkPairMorphism papb qaqb) (MkPairMorphism pbpa qbqa) =
        MkPairMorphism (mkKindIsomorphism @_ @cat papb pbpa) (mkKindIsomorphism @_ @cat qaqb qbqa)
