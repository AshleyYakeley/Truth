module Language.Expression.Arguments where

import Shapes

data Arguments (w :: Type -> Type) (f :: k) (t :: Type) where
    NilArguments :: Arguments w t t
    ConsArguments :: w a -> Arguments w (f a) t -> Arguments w f t

instance TestEquality w => TestEquality (Arguments w f) where
    testEquality NilArguments NilArguments = Just Refl
    testEquality (ConsArguments w1 args1) (ConsArguments w2 args2) = do
        Refl <- testEquality w1 w2
        Refl <- testEquality args1 args2
        Just Refl

data ConcreteType (w :: forall k. k -> Type) (t :: Type) where
    MkConcreteType
        :: forall (w :: forall k. k -> Type) (k :: Type) (f :: k) (t :: Type).
           w f
        -> Arguments (ConcreteType w) f t
        -> ConcreteType w t

instance forall (w :: forall k. k -> Type). TestHetEquality w => TestEquality (ConcreteType w) where
    testEquality (MkConcreteType gt1 args1) (MkConcreteType gt2 args2) = do
        HRefl <- testHetEquality gt1 gt2
        Refl <- testEquality args1 args2
        return Refl
