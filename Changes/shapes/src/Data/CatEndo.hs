module Data.CatEndo where

import Shapes.Import

type CatEndo :: forall k. (k -> k -> Type) -> k -> Type
newtype CatEndo cat a = MkCatEndo
    { unCatEndo :: cat a a
    }

instance forall k (cat :: k -> k -> Type) (a :: k). Category cat => Semigroup (CatEndo cat a) where
    MkCatEndo p <> MkCatEndo q = MkCatEndo $ p . q

instance forall k (cat :: k -> k -> Type) (a :: k). Category cat => Monoid (CatEndo cat a) where
    mempty = MkCatEndo id
