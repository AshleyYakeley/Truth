{-# OPTIONS -Wno-redundant-constraints #-}

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

unsafeAssign :: forall k (a :: k) (b :: k) r. (a ~ b => r) -> r
unsafeAssign call =
    case unsafeRefl @k @a @b of
        Refl -> call

unsafeId ::
       forall k (cat :: k -> k -> Type) (a :: k) (b :: k). Category cat
    => cat a b
unsafeId = unsafeAssign @k @a @b $ id @cat @a

unsafeHRefl :: forall ka (a :: ka) kb (b :: kb). a :~~: b
unsafeHRefl = unsafeAssign @Type @ka @kb $ unsafeAssign @ka @a @b $ HRefl

unsafeCheatHetEquality :: forall (w :: forall k. k -> Type) ka (a :: ka) kb (b :: kb). w a -> w b -> a :~~: b
unsafeCheatHetEquality _ _ = unsafeHRefl @ka @a @kb @b

unsafeAssignWit :: forall k (a :: k) (b :: k) (w :: k -> Type) r. w b -> (a ~ b => r) -> r
unsafeAssignWit _ call = unsafeAssign @k @a @b $ call

unsafeAssignWitT :: forall (a :: Type) (b :: Type) (w :: Type -> Type) r. w b -> (a ~ b => r) -> r
unsafeAssignWitT = unsafeAssignWit @Type @a @b @w

unsafeDerive1 ::
       forall k (a :: k) (b :: k) (c :: k -> Constraint). Coercible a b
    => Dict (c a)
    -> Dict (c b)
unsafeDerive1 = unsafeCoerce
