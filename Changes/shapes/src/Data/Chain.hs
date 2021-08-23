module Data.Chain where

import Shapes.Import

type Chain :: forall k. (k -> k -> Type) -> k -> Type
newtype Chain cat a =
    MkChain (cat a a)

instance forall k (cat :: k -> k -> Type) (a :: k). Category cat => Semigroup (Chain cat a) where
    MkChain p <> MkChain q = MkChain $ p . q

instance forall k (cat :: k -> k -> Type) (a :: k). Category cat => Monoid (Chain cat a) where
    mempty = MkChain id
