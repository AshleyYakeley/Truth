module Data.KindMorphism where

import Data.Bijection
import Shapes.Import

class HasKindMorphism (k :: Type) where
    type KindMorphism k (cat :: Type -> Type -> Type) :: k -> k -> Type
    mkKindBijection :: forall (a :: k) (b :: k). KindFunction k a b -> KindFunction k b a -> KindBijection k a b

type KindFunction k (a :: k) (b :: k) = KindMorphism k (->) a b

type KindBijection k (a :: k) (b :: k) = KindMorphism k Bijection a b

instance HasKindMorphism Type where
    type KindMorphism Type cat = cat
    mkKindBijection = MkBijection

newtype NestedMorphism (cat :: Type -> Type -> Type) (a :: kp -> kq) (b :: kp -> kq) = MkNestedMorphism
    { unNestedMorphism :: forall (p :: kp). KindMorphism kq cat (a p) (b p)
    }

instance Category (KindMorphism kq cat) => Category (NestedMorphism cat :: (kp -> kq) -> (kp -> kq) -> Type) where
    id = MkNestedMorphism id
    (MkNestedMorphism f) . (MkNestedMorphism g) = MkNestedMorphism $ f . g

instance HasKindMorphism kq => HasKindMorphism (kp -> kq) where
    type KindMorphism (kp -> kq) cat = NestedMorphism cat
    mkKindBijection (MkNestedMorphism ab) (MkNestedMorphism ba) = MkNestedMorphism $ mkKindBijection ab ba

newtype ConstraintMorphism (cat :: Type -> Type -> Type) (a :: Constraint) (b :: Constraint) =
    MkConstraintMorphism (cat (Dict a) (Dict b))

instance HasKindMorphism Constraint where
    type KindMorphism Constraint cat = ConstraintMorphism cat
    mkKindBijection (MkConstraintMorphism ab) (MkConstraintMorphism ba) = MkConstraintMorphism $ MkBijection ab ba

instance Category cat => Category (ConstraintMorphism cat) where
    id = MkConstraintMorphism id
    (MkConstraintMorphism f) . (MkConstraintMorphism g) = MkConstraintMorphism $ f . g

data PairMorphism (cat :: Type -> Type -> Type) (a :: (kp, kq)) (b :: (kp, kq)) where
    MkPairMorphism :: KindMorphism kp cat pa pb -> KindMorphism kq cat qa qb -> PairMorphism cat '( pa, qa) '( pb, qb)

instance (HasKindMorphism kp, HasKindMorphism kq) => HasKindMorphism (kp, kq) where
    type KindMorphism (kp, kq) cat = PairMorphism cat
    mkKindBijection (MkPairMorphism papb qaqb) (MkPairMorphism pbpa qbqa) =
        MkPairMorphism (mkKindBijection papb pbpa) (mkKindBijection qaqb qbqa)
