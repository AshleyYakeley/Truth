module Data.KindMorphism where

import Shapes.Import
import Unsafe.Refl

type family KindMorphism (cat :: Type -> Type -> Type) :: k -> k -> Type

type instance KindMorphism cat = cat

type KindFunction :: forall {k}. k -> k -> Type
type KindFunction = KindMorphism (->)

newtype NestedMorphism (cat :: Type -> Type -> Type) (a :: kp -> kq) (b :: kp -> kq) = MkNestedMorphism
    { unNestedMorphism :: forall (p :: kp). KindMorphism cat (a p) (b p)
    }

instance
    Category (KindMorphism cat :: kq -> kq -> Type) =>
    Category (NestedMorphism cat :: (kp -> kq) -> (kp -> kq) -> Type)
    where
    id = MkNestedMorphism id
    (MkNestedMorphism f) . (MkNestedMorphism g) = MkNestedMorphism $ f . g

type instance KindMorphism cat = NestedMorphism cat

type family Fst (a :: (kp, kq)) :: kp where
    Fst '(p, _) = p

type family Snd (a :: (kp, kq)) :: kq where
    Snd '(_, q) = q

unsafeTypeIsPair :: forall kp kq (a :: (kp, kq)). a :~: '(Fst a, Snd a)
unsafeTypeIsPair = unsafeRefl @(kp, kq) @a @'(Fst a, Snd a)

type PairMorphism :: (Type -> Type -> Type) -> forall kp kq. (kp, kq) -> (kp, kq) -> Type
data PairMorphism cat a b
    = MkPairMorphism
        (KindMorphism cat (Fst a) (Fst b))
        (KindMorphism cat (Snd a) (Snd b))

instance
    (Category (KindMorphism cat :: kp -> kp -> Type), Category (KindMorphism cat :: kq -> kq -> Type)) =>
    Category (PairMorphism cat :: (kp, kq) -> (kp, kq) -> Type)
    where
    id :: forall (a :: (kp, kq)). PairMorphism cat a a
    id = MkPairMorphism id id
    (.) ::
        forall (a :: (kp, kq)) (b :: (kp, kq)) (c :: (kp, kq)).
        PairMorphism cat b c ->
        PairMorphism cat a b ->
        PairMorphism cat a c
    (MkPairMorphism fp fq) . (MkPairMorphism gp gq) = MkPairMorphism (fp . gp) (fq . gq)

type instance KindMorphism cat = PairMorphism cat
