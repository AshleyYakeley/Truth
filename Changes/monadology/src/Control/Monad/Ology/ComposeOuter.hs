module Control.Monad.Ology.ComposeOuter where

import Control.Monad.Ology.MonadOuter
import Control.Monad.Ology.Trans.Constraint
import Import

type ComposeOuter :: (Type -> Type) -> (Type -> Type) -> Type -> Type
newtype ComposeOuter outer inner a = MkComposeOuter
    { getComposeOuter :: outer (inner a)
    }

instance (Foldable inner, Foldable outer, Functor outer) => Foldable (ComposeOuter outer inner) where
    foldMap am (MkComposeOuter oia) = foldMap id $ fmap (foldMap am) oia

instance (Traversable inner, Traversable outer) => Traversable (ComposeOuter outer inner) where
    traverse afb (MkComposeOuter oia) = fmap MkComposeOuter $ traverse (traverse afb) oia

instance Traversable outer => TransConstraint Traversable (ComposeOuter outer) where
    hasTransConstraint = Dict

instance (Functor inner, Functor outer) => Functor (ComposeOuter outer inner) where
    fmap ab (MkComposeOuter oia) = MkComposeOuter $ fmap (fmap ab) oia

instance Functor outer => TransConstraint Functor (ComposeOuter outer) where
    hasTransConstraint = Dict

instance (Applicative inner, Applicative outer) => Applicative (ComposeOuter outer inner) where
    pure a = MkComposeOuter $ pure $ pure a
    MkComposeOuter mab <*> MkComposeOuter ma = MkComposeOuter $ liftA2 (<*>) mab ma

instance Applicative outer => TransConstraint Applicative (ComposeOuter outer) where
    hasTransConstraint = Dict

instance (Monad inner, MonadOuter outer) => Monad (ComposeOuter outer inner) where
    return = pure
    MkComposeOuter oia >>= f =
        MkComposeOuter $ do
            ia <- oia
            fmap (\iib -> iib >>= id) $ outerCommute $ fmap (getComposeOuter . f) ia

instance MonadOuter outer => TransConstraint Monad (ComposeOuter outer) where
    hasTransConstraint = Dict

liftOuter :: (Functor outer, Applicative inner) => outer a -> ComposeOuter outer inner a
liftOuter oa = MkComposeOuter $ fmap pure oa

instance MonadOuter outer => MonadTrans (ComposeOuter outer) where
    lift ma = MkComposeOuter $ pure ma

instance (MonadOuter outer, MonadIO inner) => MonadIO (ComposeOuter outer inner) where
    liftIO ioa = lift $ liftIO ioa

instance MonadOuter outer => TransConstraint MonadIO (ComposeOuter outer) where
    hasTransConstraint = Dict
