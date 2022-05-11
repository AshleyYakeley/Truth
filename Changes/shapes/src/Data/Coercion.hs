module Data.Coercion where

import Control.Category.Dual
import Control.Category.Groupoid
import Data.CatFunctor
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

applyCoercion1 ::
       forall f g a b. (RepresentationalRole f)
    => Coercion f g
    -> Coercion a b
    -> Coercion (f a) (g b)
applyCoercion1 MkCoercion cab =
    case representationalCoercion @_ @_ @f cab of
        MkCoercion -> MkCoercion

applyCoercion2 ::
       forall f g a b. (RepresentationalRole g)
    => Coercion f g
    -> Coercion a b
    -> Coercion (f a) (g b)
applyCoercion2 MkCoercion cab =
    case representationalCoercion @_ @_ @g cab of
        MkCoercion -> MkCoercion

coerceCoercion :: (Coercible a1 a2, Coercible b1 b2) => Coercion a1 b1 -> Coercion a2 b2
coerceCoercion c = MkCoercion . c . MkCoercion

coerce' ::
       forall k (a :: k) (b :: k). (CoercibleKind k)
    => Coercible a b => KindFunction a b
coerce' = coercionToFunction MkCoercion

coerceIsomorphism ::
       forall k (cat :: k -> k -> Type) (a :: k) (b :: k).
       (Category cat, RepresentationalRole (cat a), RepresentationalRole (cat b), Coercible a b)
    => Isomorphism cat a b
coerceIsomorphism =
    MkIsomorphism
        (coercionToFunction (representationalCoercion MkCoercion) id)
        (coercionToFunction (representationalCoercion MkCoercion) id)

coerceUnsafeCoerce :: Coercible a b => a :~: b
coerceUnsafeCoerce = unsafeCoerce Refl

coercionUnsafeCoerce :: Coercion a b -> a :~: b
coercionUnsafeCoerce MkCoercion = coerceUnsafeCoerce

isoCoerce ::
       forall (f :: Type -> Type) a b. (IsoVariant f, Coercible a b)
    => f a
    -> f b
isoCoerce = isoMap coerce coerce

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

class RepresentationalRole (f :: kp -> kq) where
    representationalCoercion :: forall (a :: kp) (b :: kp). Coercion a b -> Coercion (f a) (f b)

class RepresentationalRole f => PhantomRole (f :: kp -> kq) where
    phantomCoercion :: forall (a :: kp) (b :: kp). Coercion (f a) (f b)

instance RepresentationalRole f => CatFunctor Coercion Coercion (f :: kp -> kq) where
    cfmap = representationalCoercion

instance RepresentationalRole f => CatFunctor (CatDual Coercion) Coercion (f :: kp -> kq) where
    cfmap (MkCatDual ab) = invert $ representationalCoercion ab

instance RepresentationalRole Identity where
    representationalCoercion MkCoercion = MkCoercion

instance RepresentationalRole Maybe where
    representationalCoercion MkCoercion = MkCoercion

instance RepresentationalRole [] where
    representationalCoercion MkCoercion = MkCoercion

instance RepresentationalRole NonEmpty where
    representationalCoercion MkCoercion = MkCoercion

instance RepresentationalRole (->) where
    representationalCoercion MkCoercion = MkCoercion

instance RepresentationalRole ((->) a) where
    representationalCoercion MkCoercion = MkCoercion

instance RepresentationalRole (,) where
    representationalCoercion MkCoercion = MkCoercion

instance RepresentationalRole ((,) a) where
    representationalCoercion MkCoercion = MkCoercion

instance RepresentationalRole Either where
    representationalCoercion MkCoercion = MkCoercion

instance RepresentationalRole (Either a) where
    representationalCoercion MkCoercion = MkCoercion

instance RepresentationalRole Vector where
    representationalCoercion MkCoercion = MkCoercion

instance RepresentationalRole IO where
    representationalCoercion MkCoercion = MkCoercion

instance RepresentationalRole m => RepresentationalRole (LifeCycleT m) where
    representationalCoercion c =
        case representationalCoercion @_ @_ @m c of
            MkCoercion -> MkCoercion

instance RepresentationalRole ReaderT where
    representationalCoercion MkCoercion = MkCoercion

instance RepresentationalRole (ReaderT r) where
    representationalCoercion MkCoercion = MkCoercion

instance RepresentationalRole m => RepresentationalRole (ReaderT r m) where
    representationalCoercion cab =
        case representationalCoercion @_ @_ @m cab of
            MkCoercion -> MkCoercion

instance RepresentationalRole (WriterT w) where
    representationalCoercion MkCoercion = MkCoercion

instance RepresentationalRole m => RepresentationalRole (WriterT w m) where
    representationalCoercion (cab :: Coercion a b) =
        case representationalCoercion @_ @_ @(,) cab of
            MkCoercion ->
                case representationalCoercion @_ @_ @m (MkCoercion @_ @(a, w) @(b, w)) of
                    MkCoercion -> MkCoercion

instance RepresentationalRole (StateT s) where
    representationalCoercion MkCoercion = MkCoercion

instance RepresentationalRole m => RepresentationalRole (StateT s m) where
    representationalCoercion (cab :: Coercion a b) =
        case representationalCoercion @_ @_ @(,) cab of
            MkCoercion ->
                case representationalCoercion @_ @_ @m (MkCoercion @_ @(a, s) @(b, s)) of
                    MkCoercion -> MkCoercion

instance RepresentationalRole (ComposeInner inner) where
    representationalCoercion MkCoercion = MkCoercion

instance (RepresentationalRole inner, RepresentationalRole outer) => RepresentationalRole (ComposeInner inner outer) where
    representationalCoercion cab =
        case representationalCoercion @_ @_ @outer $ representationalCoercion @_ @_ @inner cab of
            MkCoercion -> MkCoercion
