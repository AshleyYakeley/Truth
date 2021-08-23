{-# OPTIONS -fno-warn-orphans #-}

module Data.Compose where

import Shapes.Import

deriving instance Semigroup (p (q a)) => Semigroup (Compose p q a)

deriving instance Monoid (p (q a)) => Monoid (Compose p q a)
