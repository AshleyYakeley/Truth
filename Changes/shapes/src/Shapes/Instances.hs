{-# OPTIONS -fno-warn-orphans #-}

module Shapes.Instances
    (
    ) where

-- orphan instances for imported packages
import Shapes.Import

instance Empty a => Countable (NonEmpty a) where
    countPrevious = finiteCountPrevious
    countMaybeNext = finiteCountMaybeNext

instance Empty a => Searchable (NonEmpty a) where
    search = finiteSearch

instance Empty a => Finite (NonEmpty a) where
    allValues = []

instance Empty a => Empty (NonEmpty a) where
    never (a :| _) = never a

deriving instance Semigroup (m b) => Semigroup (Kleisli m a b)

deriving instance Monoid (m b) => Monoid (Kleisli m a b)

deriving instance
         Eq (outer (inner a)) => Eq (Compose outer inner a)
