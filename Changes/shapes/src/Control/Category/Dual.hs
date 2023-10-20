module Control.Category.Dual where

import Control.Category.Groupoid
import Shapes.Import

newtype CatDual (cat :: k -> k -> Type) (a :: k) (b :: k) = MkCatDual
    { unCatDual :: cat b a
    }

instance Category cat => Category (CatDual cat) where
    id = MkCatDual id
    (MkCatDual p) . (MkCatDual q) = MkCatDual $ q . p

instance Groupoid cat => Groupoid (CatDual cat) where
    invert (MkCatDual c) = MkCatDual $ invert c

instance Show (cat b a) => Show (CatDual cat a b) where
    show (MkCatDual c) = show c
