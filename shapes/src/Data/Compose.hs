{-# OPTIONS -fno-warn-orphans #-}

module Data.Compose where

import Shapes.Import

deriving instance Semigroup (p (q a)) => Semigroup (Compose p q a)

deriving instance Monoid (p (q a)) => Monoid (Compose p q a)

instance (Monad p, Monad q, Traversable q) => Monad (Compose p q) where
    return = pure
    (Compose pqa) >>= f =
        Compose $ do
            qa <- pqa
            qqb <- traverse (getCompose . f) qa
            return $ qqb >>= id

liftOuter :: (Functor m, Applicative n) => m a -> Compose m n a
liftOuter ma = Compose $ fmap pure ma

liftInner :: (Applicative m) => n a -> Compose m n a
liftInner na = Compose $ pure na

instance (MonadIO p, Monad q, Traversable q) => MonadIO (Compose p q) where
    liftIO ioa = liftOuter $ liftIO ioa
