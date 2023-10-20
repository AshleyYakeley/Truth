{-# OPTIONS -fno-warn-orphans #-}

module Data.Coerce.Role where

import Control.Category.Dual
import Control.Category.Groupoid
import Control.Stream
import Data.CatFunctor
import Data.Coerce.Coercion
import Data.Isomorphism
import Shapes.Import

class RepresentationalRole (f :: kp -> kq) where
    representationalCoercion :: forall (a :: kp) (b :: kp). Coercion a b -> Coercion (f a) (f b)

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

coerceIsomorphism ::
       forall k (cat :: k -> k -> Type) (a :: k) (b :: k).
       (Category cat, RepresentationalRole (cat a), RepresentationalRole (cat b), Coercible a b)
    => Isomorphism cat a b
coerceIsomorphism =
    MkIsomorphism
        (coercionToFunction (representationalCoercion MkCoercion) id)
        (coercionToFunction (representationalCoercion MkCoercion) id)

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

instance RepresentationalRole (Map k) where
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

instance RepresentationalRole m => RepresentationalRole (LifecycleT m) where
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

instance RepresentationalRole ItemOrEnd where
    representationalCoercion MkCoercion = MkCoercion

class RepresentationalRole f => PhantomRole (f :: kp -> kq) where
    phantomCoercion :: forall (a :: kp) (b :: kp). Coercion (f a) (f b)
