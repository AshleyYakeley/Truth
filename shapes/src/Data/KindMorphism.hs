module Data.KindMorphism where

import Control.Category.Dual
import Control.Category.Groupoid
import Data.Witness.Kind
import Shapes.Import

type family KindMorphism (cat :: Type -> Type -> Type) :: k -> k -> Type

type instance KindMorphism cat = cat

type KindFunction = KindMorphism (->)

newtype NestedMorphism (cat :: Type -> Type -> Type) (a :: kp -> kq) (b :: kp -> kq) = MkNestedMorphism
    { unNestedMorphism :: forall (p :: kp). InKind p => KindMorphism cat (a p) (b p)
    }

instance Category (KindMorphism cat :: kq -> kq -> Type) =>
             Category (NestedMorphism cat :: (kp -> kq) -> (kp -> kq) -> Type) where
    id = MkNestedMorphism id
    (MkNestedMorphism f) . (MkNestedMorphism g) = MkNestedMorphism $ f . g

type instance KindMorphism cat = NestedMorphism cat

newtype ConstraintMorphism (cat :: Type -> Type -> Type) (a :: Constraint) (b :: Constraint) =
    MkConstraintMorphism (cat (Dict a) (Dict b))

type instance KindMorphism cat = ConstraintMorphism cat

instance Category cat => Category (ConstraintMorphism cat) where
    id = MkConstraintMorphism id
    (MkConstraintMorphism f) . (MkConstraintMorphism g) = MkConstraintMorphism $ f . g

data PairMorphism (cat :: Type -> Type -> Type) (a :: (kp, kq)) (b :: (kp, kq)) where
    MkPairMorphism :: KindMorphism cat pa pb -> KindMorphism cat qa qb -> PairMorphism cat '( pa, qa) '( pb, qb)

instance (InCategory (KindMorphism cat :: kp -> kp -> Type), InCategory (KindMorphism cat :: kq -> kq -> Type)) =>
             InCategory (PairMorphism cat :: (kp, kq) -> (kp, kq) -> Type) where
    cid :: forall a. InKind a
        => PairMorphism cat a a
    cid =
        case inKind @_ @a of
            MkPairWitness -> MkPairMorphism cid cid
    (<.>) ::
           forall a b c. (InKind a, InKind b, InKind c)
        => PairMorphism cat b c
        -> PairMorphism cat a b
        -> PairMorphism cat a c
    (MkPairMorphism fp fq) <.> (MkPairMorphism gp gq) =
        case (inKind @_ @a, inKind @_ @b, inKind @_ @c) of
            (MkPairWitness, MkPairWitness, MkPairWitness) -> MkPairMorphism (fp <.> gp) (fq <.> gq)

type instance KindMorphism cat = PairMorphism cat

class InCategory (cat :: k -> k -> Type) where
    cid :: InKind a => cat a a
    default cid :: Category cat => cat a a
    cid = id
    (<.>) :: (InKind a, InKind b, InKind c) => cat b c -> cat a b -> cat a c
    default (<.>) :: Category cat => cat b c -> cat a b -> cat a c
    (<.>) = (.)

instance InCategory (->)

instance InCategory (:~:)

instance InCategory (:~~:)

instance InCategory (KindMorphism cat :: kq -> kq -> Type) =>
             InCategory (NestedMorphism cat :: (kp -> kq) -> (kp -> kq) -> Type) where
    cid :: forall a. InKind a
        => NestedMorphism cat a a
    cid =
        case inKind @_ @a of
            MkFunctionKindWitness -> MkNestedMorphism cid
    (<.>) ::
           forall a b c. (InKind a, InKind b, InKind c)
        => NestedMorphism cat b c
        -> NestedMorphism cat a b
        -> NestedMorphism cat a c
    (MkNestedMorphism f) <.> (MkNestedMorphism g) =
        case (inKind @_ @a, inKind @_ @b, inKind @_ @c) of
            (MkFunctionKindWitness, MkFunctionKindWitness, MkFunctionKindWitness) -> MkNestedMorphism $ f <.> g

instance InCategory cat => InCategory (CatDual cat) where
    cid = MkCatDual cid
    MkCatDual cb <.> MkCatDual ba = MkCatDual $ ba <.> cb

class InCategory cat => InGroupoid (cat :: k -> k -> Type) where
    cinvert ::
           forall (a :: k) (b :: k). (InKind a, InKind b)
        => cat a b
        -> cat b a
    default cinvert :: Groupoid cat => cat a b -> cat b a
    cinvert = invert

instance InGroupoid (:~:)

instance InGroupoid (:~~:)
