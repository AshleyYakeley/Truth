module Data.Shim.Mono.Arguments where

import Shapes

type Arguments :: forall k. (Type -> Type) -> k -> Type -> Type
data Arguments w f t where
    NilArguments :: Arguments w t t
    ConsArguments :: w a -> Arguments w (f a) t -> Arguments w f t

instance TestEquality w => TestEquality (Arguments w f) where
    testEquality NilArguments NilArguments = Just Refl
    testEquality (ConsArguments w1 args1) (ConsArguments w2 args2) = do
        Refl <- testEquality w1 w2
        Refl <- testEquality args1 args2
        Just Refl

data MonoType (w :: forall k. k -> Type) (t :: Type) where
    MkMonoType
        :: forall (w :: forall k. k -> Type) (k :: Type) (f :: k) (t :: Type).
           w f
        -> Arguments (MonoType w) f t
        -> MonoType w t

instance forall (w :: forall k. k -> Type). TestHetEquality w => TestEquality (MonoType w) where
    testEquality (MkMonoType gt1 args1) (MkMonoType gt2 args2) = do
        HRefl <- testHetEquality gt1 gt2
        Refl <- testEquality args1 args2
        return Refl

mapArgumentsM :: Applicative m => (forall a. w1 a -> m (w2 a)) -> Arguments w1 f t -> m (Arguments w2 f t)
mapArgumentsM _ NilArguments = pure NilArguments
mapArgumentsM ff (ConsArguments w args) = ConsArguments <$> (ff w) <*> mapArgumentsM ff args

mapArguments :: (forall a. w1 a -> w2 a) -> Arguments w1 f t -> Arguments w2 f t
mapArguments _ NilArguments = NilArguments
mapArguments ff (ConsArguments w args) = ConsArguments (ff w) $ mapArguments ff args
