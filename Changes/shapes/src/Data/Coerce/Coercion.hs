module Data.Coerce.Coercion where

import Control.Category.Groupoid
import Data.Isomorphism
import Data.KindMorphism
import Shapes.Import
import Unsafe.Coerce

data Coercion (a :: k) (b :: k) where
    MkCoercion
        :: forall k (a :: k) (b :: k). Coercible a b
        => Coercion a b

instance Category Coercion where
    id = MkCoercion
    MkCoercion . MkCoercion = MkCoercion

instance Groupoid Coercion where
    invert MkCoercion = MkCoercion

class (HasKindMorphism k, Category (KindFunction :: k -> k -> Type)) => CoercibleKind k where
    coercionToKindMorphism :: forall (a :: k) (b :: k). Coercion a b -> KindMorphism Coercion a b

coercionToFunction ::
       forall k (a :: k) (b :: k). (CoercibleKind k)
    => Coercion a b
    -> KindFunction a b
coercionToFunction c = kindMorphismMapCat @_ @Coercion @(->) (\MkCoercion -> coerce) $ coercionToKindMorphism c

coerceCoercion :: (Coercible a1 a2, Coercible b1 b2) => Coercion a1 b1 -> Coercion a2 b2
coerceCoercion c = MkCoercion . c . MkCoercion

coerce' ::
       forall k (a :: k) (b :: k). (CoercibleKind k)
    => Coercible a b => KindFunction a b
coerce' = coercionToFunction MkCoercion

coerceUnsafeCoerce :: Coercible a b => a :~: b
coerceUnsafeCoerce = unsafeCoerce Refl

coercionUnsafeCoerce :: Coercion a b -> a :~: b
coercionUnsafeCoerce MkCoercion = coerceUnsafeCoerce

isoCoerce ::
       forall (f :: Type -> Type) a b. (Invariant f, Coercible a b)
    => f a
    -> f b
isoCoerce = invmap coerce coerce

instance CoercibleKind Type where
    coercionToKindMorphism c = c

instance (CoercibleKind kq) => CoercibleKind (kp -> kq) where
    coercionToKindMorphism :: forall (a :: kp -> kq) (b :: kp -> kq). Coercion a b -> NestedMorphism Coercion a b
    coercionToKindMorphism MkCoercion = MkNestedMorphism $ coercionToKindMorphism MkCoercion

instance (CoercibleKind kp, CoercibleKind kq) => CoercibleKind (kp, kq) where
    coercionToKindMorphism :: forall (a :: (kp, kq)) (b :: (kp, kq)). Coercion a b -> KindMorphism Coercion a b
    coercionToKindMorphism MkCoercion =
        case (unsafeTypeIsPair @_ @_ @a, unsafeTypeIsPair @_ @_ @b) of
            (Refl, Refl) -> MkPairMorphism (coercionToKindMorphism MkCoercion) (coercionToKindMorphism MkCoercion)
