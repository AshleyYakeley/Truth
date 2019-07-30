module Control.Category.Dual where

import Shapes.Import

newtype CatDual (cat :: k -> k -> Type) (a :: k) (b :: k) =
    MkCatDual (cat b a)
    deriving (Show)

instance Category cat => Category (CatDual cat) where
    id = MkCatDual id
    (MkCatDual p) . (MkCatDual q) = MkCatDual $ q . p
