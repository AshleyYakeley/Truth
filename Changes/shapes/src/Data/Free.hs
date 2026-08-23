module Data.Free where

import Control.Alternative.Free qualified
import Control.Applicative.Free qualified

import Data.KindMorphism
import Shapes.Import

class (forall (t :: k). c (Free c t)) => HasFree (c :: k -> Constraint) where
    type Free c :: k -> k
    toFree :: forall (t :: k). KindFunction t (Free c t)
    fromFree :: forall (t :: k). c t => KindFunction (Free c t) t

toFree1 :: forall k1 k2 (c :: (k1 -> k2) -> Constraint) (a :: k1 -> k2) (p :: k1). HasFree c => KindFunction (a p) (Free c a p)
toFree1 = unNestedMorphism $ toFree @(k1 -> k2) @c

fromFree1 :: forall k1 k2 (c :: (k1 -> k2) -> Constraint) (a :: k1 -> k2) (p :: k1). (HasFree c, c a) => KindFunction (Free c a p) (a p)
fromFree1 = unNestedMorphism $ fromFree @(k1 -> k2) @c

instance HasFree Semigroup where
    type Free Semigroup = NonEmpty
    toFree x = x :| []
    fromFree = sconcat

instance HasFree Monoid where
    type Free Monoid = []
    toFree x = [x]
    fromFree = mconcat

data FreeFunctor (f :: Type -> Type) (x :: Type) where
    MkFreeFunctor :: f a -> (a -> b) -> FreeFunctor f b

instance Functor (FreeFunctor f) where
    fmap bc (MkFreeFunctor fa ab) = MkFreeFunctor fa (bc . ab)

instance HasFree Functor where
    type Free Functor = FreeFunctor
    toFree = MkNestedMorphism $ \x -> MkFreeFunctor x id
    fromFree = MkNestedMorphism $ \(MkFreeFunctor x f) -> fmap f x

type FreeApplicative = Control.Applicative.Free.Ap

pattern PureFreeApplicative :: a -> FreeApplicative f a
pattern PureFreeApplicative a = Control.Applicative.Free.Pure a

pattern ApFreeApplicative :: f a -> FreeApplicative f (a -> b) -> FreeApplicative f b
pattern ApFreeApplicative fa af = Control.Applicative.Free.Ap fa af

{-# COMPLETE PureFreeApplicative, ApFreeApplicative #-}

instance HasFree Applicative where
    type Free Applicative = FreeApplicative
    toFree = MkNestedMorphism Control.Applicative.Free.liftAp
    fromFree = MkNestedMorphism Control.Applicative.Free.retractAp

instance HasFree Alternative where
    type Free Alternative = Control.Alternative.Free.Alt
    toFree = MkNestedMorphism Control.Alternative.Free.liftAlt
    fromFree = MkNestedMorphism $ Control.Alternative.Free.runAlt id

newtype FreeMonad f a = MkFreeMonad (forall r. (a -> r) -> (forall t. f t -> (t -> r) -> r) -> r)

instance Functor (FreeMonad f) where
    fmap ab (MkFreeMonad fff) = MkFreeMonad $ \br ftt -> fff (br . ab) ftt

instance Applicative (FreeMonad f) where
    pure a = MkFreeMonad $ \ar _ -> ar a
    MkFreeMonad ffab <*> MkFreeMonad ffa = MkFreeMonad $ \br ftt -> ffab (\ab -> ffa (br . ab) ftt) ftt

instance Monad (FreeMonad f) where
    MkFreeMonad ffa >>= amb = MkFreeMonad $ \br ftt -> ffa (\a -> case amb a of MkFreeMonad ffb -> ffb br ftt) ftt

instance HasFree Monad where
    type Free Monad = FreeMonad
    toFree = MkNestedMorphism $ \fa -> MkFreeMonad $ \ar ftt -> ftt fa ar
    fromFree = MkNestedMorphism $ \(MkFreeMonad fff) -> fff return (>>=)
