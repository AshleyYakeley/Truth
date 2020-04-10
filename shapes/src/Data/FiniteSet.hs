module Data.FiniteSet where

import Data.Filterable
import Data.KeyContainer
import qualified Data.List as List
import Shapes.Import

newtype FiniteSet a = MkFiniteSet
    { unFiniteSet :: [a]
    } deriving ( Foldable
               , Functor
               , Applicative -- ,Monad,Alternative,MonadPlus,
               , MonoFunctor
               , MonoFoldable
               , GrowingAppend
               , Filterable
               )

instance Eq a => Semigroup (FiniteSet a) where
    (<>) = union

instance Eq a => Monoid (FiniteSet a) where
    mempty = MkFiniteSet mempty

instance Show a => Show (FiniteSet a) where
    show (MkFiniteSet aa) = show aa

sumFiniteSet :: FiniteSet a -> FiniteSet b -> FiniteSet (Either a b)
sumFiniteSet (MkFiniteSet aa) (MkFiniteSet bb) = MkFiniteSet $ fmap Left aa <> fmap Right bb

type instance Element (FiniteSet a) = a

instance Traversable FiniteSet where
    traverse afb (MkFiniteSet aa) = fmap MkFiniteSet $ traverse afb aa

instance MonoTraversable (FiniteSet a)

instance Eq a => SetContainer (FiniteSet a) where
    type ContainerKey (FiniteSet a) = a
    member key (MkFiniteSet aa) = elem key aa
    notMember key set = not $ member key set
    union (MkFiniteSet a) (MkFiniteSet b) = MkFiniteSet $ List.union a b
    difference (MkFiniteSet a) (MkFiniteSet b) = MkFiniteSet $ a List.\\ b
    intersection (MkFiniteSet a) (MkFiniteSet b) = MkFiniteSet $ List.intersect a b
    keys (MkFiniteSet a) = a

instance Eq a => IsSet (FiniteSet a) where
    insertSet = insertItem
    deleteSet = deleteKey
    singletonSet = MkFiniteSet . pure
    setFromList = MkFiniteSet . nub
    setToList = unFiniteSet

instance MonoPointed (FiniteSet a) where
    opoint = MkFiniteSet . pure

instance Eq a => Lattice (FiniteSet a) where
    (/\) = intersection
    (\/) = union

instance Eq a => BoundedJoinSemiLattice (FiniteSet a) where
    bottom = mempty

instance Eq a => ItemContainer (FiniteSet a)

instance Eq a => KeyContainer (FiniteSet a) where
    itemKey a = a
    lookupItem key = List.find (\k -> k == key)
    insertItem e (MkFiniteSet []) = MkFiniteSet [e]
    insertItem e (MkFiniteSet (a:aa))
        | e == a = MkFiniteSet $ e : aa
    insertItem e (MkFiniteSet (a:aa)) = MkFiniteSet $ a : (unFiniteSet $ insertItem e $ MkFiniteSet aa)
    deleteKey _ (MkFiniteSet []) = MkFiniteSet []
    deleteKey k (MkFiniteSet (k':aa))
        | k == k' = MkFiniteSet $ aa
    deleteKey k (MkFiniteSet (a:aa)) = MkFiniteSet $ a : (unFiniteSet $ deleteKey k $ MkFiniteSet aa)
    fromItemList = MkFiniteSet

instance (Eq key, Random key) => IONewItemKeyContainer (FiniteSet key) where
    newKeyContainerItem = randomIO

maybePoint :: (Monoid l, MonoPointed l, Element l ~ a) => Maybe a -> l
maybePoint Nothing = mempty
maybePoint (Just a) = opoint a
