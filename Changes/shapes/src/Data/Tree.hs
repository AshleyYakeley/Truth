module Data.Tree where

import Shapes.Import

data Tree a = MkTree
    { treeRoot :: a
    , treeBranches :: Forest a
    }

newtype Forest a =
    MkForest [Tree a]
    deriving (Semigroup, Monoid)

instance Functor Tree where
    fmap ab (MkTree r br) = MkTree (ab r) (fmap ab br)

instance Functor Forest where
    fmap ab (MkForest tt) = MkForest $ fmap (fmap ab) tt

instance Foldable Tree where
    foldMap am (MkTree r b) = am r <> foldMap am b

instance Foldable Forest where
    foldMap am (MkForest tt) = mconcat $ fmap (foldMap am) tt

instance Traversable Tree where
    traverse afb (MkTree r b) = liftA2 MkTree (afb r) $ traverse afb b

instance Traversable Forest where
    traverse afb (MkForest tt) = fmap MkForest $ sequenceA $ fmap (traverse afb) tt

instance Applicative Tree where
    pure a = MkTree a mempty
    MkTree fr (MkForest fb) <*> xt@(MkTree xr xb) =
        MkTree (fr xr) $ fmap fr xb <> (MkForest $ fmap (\ft -> ft <*> xt) fb)

instance Applicative Forest where
    pure a = MkForest $ pure $ pure a
    MkForest f <*> MkForest x = MkForest $ liftA2 (<*>) f x

instance Alternative Forest where
    empty = mempty
    (<|>) = (<>)

pureForest :: Tree a -> Forest a
pureForest t = MkForest [t]

mapForest :: (Tree a -> Tree b) -> Forest a -> Forest b
mapForest f (MkForest tt) = MkForest $ fmap f tt

combineForest :: (Tree a -> Tree b -> Tree c) -> Forest a -> Forest b -> Forest c
combineForest f (MkForest a) (MkForest b) = MkForest $ liftA2 f a b

bindForest :: Forest a -> (Tree a -> Forest b) -> Forest b
bindForest (MkForest tt) f = mconcat $ fmap f tt
