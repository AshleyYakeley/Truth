module Unsafe.Type where

import Data.Bijection
import Shapes.Import
import Unsafe.Coerce

unsafeRefl :: forall k (a :: k) (b :: k). a :~: b
unsafeRefl = unsafeCoerce $ Refl @a

unsafeGetRefl ::
       forall k (a :: k) (b :: k) (m :: Type -> Type). Applicative m
    => m (a :~: b)
unsafeGetRefl = pure $ unsafeRefl @k @a @b

unsafeCheatEquality :: forall k (w :: k -> Type) (a :: k) (b :: k). w a -> w b -> a :~: b
unsafeCheatEquality _ _ = unsafeRefl @k @a @b

unsafeId ::
       forall k (cat :: k -> k -> Type) (a :: k) (b :: k). Category cat
    => cat a b
unsafeId =
    case unsafeRefl @k @a @b of
        Refl -> id @cat @a

unsafeIsomorphism ::
       forall k (cat :: k -> k -> Type) (a :: k) (b :: k). Category cat
    => Isomorphism cat a b
unsafeIsomorphism = MkIsomorphism (unsafeId @k @cat @a @b) (unsafeId @k @cat @b @a)

unsafeGetIsomorphism ::
       forall k (cat :: k -> k -> Type) (a :: k) (b :: k) (m :: Type -> Type). (Category cat, Applicative m)
    => m (Isomorphism cat a b)
unsafeGetIsomorphism = pure $ unsafeIsomorphism @k @cat @a @b

unsafeHRefl :: forall ka (a :: ka) kb (b :: kb). a :~~: b
unsafeHRefl = unsafeCoerce $ HRefl @a

unsafeCheatHetEquality :: forall (w :: forall k. k -> Type) ka (a :: ka) kb (b :: kb). w a -> w b -> a :~~: b
unsafeCheatHetEquality _ _ = unsafeHRefl @ka @a @kb @b
