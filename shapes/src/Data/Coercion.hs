module Data.Coercion where

import Control.Category.Dual
import Control.Category.Groupoid
import Data.CatFunctor
import Data.IsoVariant
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

instance InCategory Coercion

instance Groupoid Coercion where
    invert MkCoercion = MkCoercion

instance InGroupoid Coercion

class (HasKindMorphism k, InCategory (KindFunction :: k -> k -> Type)) => CoercibleKind k where
    coercionToKindMorphism ::
           forall (a :: k) (b :: k). (InKind a, InKind b)
        => Coercion a b
        -> KindMorphism Coercion a b

coercionToFunction ::
       forall k (a :: k) (b :: k). (CoercibleKind k, InKind a, InKind b)
    => Coercion a b
    -> KindFunction a b
coercionToFunction c = kindMorphismMapCat @_ @Coercion @(->) (\MkCoercion -> coerce) $ coercionToKindMorphism c

applyCoercion1 ::
       forall f g a b. (InKind a, InKind b, RepresentationalRole f)
    => Coercion f g
    -> Coercion a b
    -> Coercion (f a) (g b)
applyCoercion1 MkCoercion cab =
    case representationalCoercion @_ @_ @f cab of
        MkCoercion -> MkCoercion

applyCoercion2 ::
       forall f g a b. (InKind a, InKind b, RepresentationalRole g)
    => Coercion f g
    -> Coercion a b
    -> Coercion (f a) (g b)
applyCoercion2 MkCoercion cab =
    case representationalCoercion @_ @_ @g cab of
        MkCoercion -> MkCoercion

coerceCoercion :: (Coercible a1 a2, Coercible b1 b2) => Coercion a1 b1 -> Coercion a2 b2
coerceCoercion c = MkCoercion . c . MkCoercion

coerce' ::
       forall k (a :: k) (b :: k). (CoercibleKind k, InKind a, InKind b)
    => Coercible a b => KindFunction a b
coerce' = coercionToFunction MkCoercion

coerceIsomorphism ::
       forall k (cat :: k -> k -> Type) (a :: k) (b :: k).
       (Category cat, InKind a, InKind b, RepresentationalRole (cat a), RepresentationalRole (cat b), Coercible a b)
    => Isomorphism cat a b
coerceIsomorphism =
    MkIsomorphism
        (coercionToFunction (representationalCoercion MkCoercion) id)
        (coercionToFunction (representationalCoercion MkCoercion) id)

coerceUnsafeCoerce :: Coercible a b => a :~: b
coerceUnsafeCoerce = unsafeCoerce Refl

coercionUnsafeCoerce :: Coercion a b -> a :~: b
coercionUnsafeCoerce MkCoercion = coerceUnsafeCoerce

isoCoerce :: (IsoVariant f, Coercible a b) => f a -> f b
isoCoerce = isoMap coerce coerce

instance CoercibleKind Type where
    coercionToKindMorphism c = c

instance CoercibleKind kq => CoercibleKind (kp -> kq) where
    coercionToKindMorphism ::
           forall (a :: kp -> kq) (b :: kp -> kq). (InKind a, InKind b)
        => Coercion a b
        -> NestedMorphism Coercion a b
    coercionToKindMorphism MkCoercion =
        case (inKind @_ @a, inKind @_ @b) of
            (MkFunctionKindWitness, MkFunctionKindWitness) -> MkNestedMorphism $ coercionToKindMorphism MkCoercion

instance (CoercibleKind kp, CoercibleKind kq) => CoercibleKind (kp, kq) where
    coercionToKindMorphism ::
           forall (a :: (kp, kq)) (b :: (kp, kq)). (InKind a, InKind b)
        => Coercion a b
        -> KindMorphism Coercion a b
    coercionToKindMorphism MkCoercion =
        case (inKind @_ @a, inKind @_ @b) of
            (MkPairWitness, MkPairWitness) ->
                MkPairMorphism (coercionToKindMorphism MkCoercion) (coercionToKindMorphism MkCoercion)

class RepresentationalRole (f :: kp -> kq) where
    representationalCoercion ::
           forall (a :: kp) (b :: kp). (InKind a, InKind b)
        => Coercion a b
        -> Coercion (f a) (f b)

class RepresentationalRole f => PhantomRole (f :: kp -> kq) where
    phantomCoercion :: forall (a :: kp) (b :: kp). Coercion (f a) (f b)

instance (RepresentationalRole f) => CatFunctor Coercion Coercion (f :: kp -> kq) where
    cfmap = representationalCoercion

instance (RepresentationalRole f) => CatFunctor (CatDual Coercion) Coercion (f :: kp -> kq) where
    cfmap (MkCatDual ab) = invert $ representationalCoercion ab

instance RepresentationalRole Identity where
    representationalCoercion MkCoercion = MkCoercion

instance RepresentationalRole Maybe where
    representationalCoercion MkCoercion = MkCoercion

instance RepresentationalRole [] where
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

instance RepresentationalRole IO where
    representationalCoercion MkCoercion = MkCoercion

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
