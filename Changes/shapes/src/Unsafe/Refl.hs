module Unsafe.Refl
    ( unsafeCoerce
    , module Unsafe.Refl
    ) where

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

unsafeHRefl :: forall ka (a :: ka) kb (b :: kb). a :~~: b
unsafeHRefl =
    case unsafeRefl @Type @ka @kb of
        Refl ->
            case unsafeRefl @ka @a @b of
                Refl -> HRefl

unsafeCheatHetEquality :: forall (w :: forall k. k -> Type) ka (a :: ka) kb (b :: kb). w a -> w b -> a :~~: b
unsafeCheatHetEquality _ _ = unsafeHRefl @ka @a @kb @b
