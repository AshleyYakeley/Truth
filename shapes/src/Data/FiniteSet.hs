module Data.FiniteSet where

import Data.Filterable
import Data.KeyContainer
import qualified Data.List as List
import Shapes.Import

newtype FiniteSet a = MkFiniteSet
    { unFiniteSet :: [a]
    } deriving ( Foldable
               , Functor -- Applicative,Monad,Alternative,MonadPlus,
               , Semigroup
               , Monoid
               , MonoFunctor
               , MonoFoldable
               , GrowingAppend
               , Filterable
               )

instance Show a => Show (FiniteSet a) where
    show (MkFiniteSet aa) = show aa

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
    insertSet = insertElement
    deleteSet = deleteElement
    singletonSet = MkFiniteSet . pure
    setFromList = MkFiniteSet . nub
    setToList = unFiniteSet

instance MonoPointed (FiniteSet a) where
    opoint = MkFiniteSet . pure

instance Eq a => JoinSemiLattice (FiniteSet a) where
    (\/) = union

instance Eq a => BoundedJoinSemiLattice (FiniteSet a) where
    bottom = mempty

instance Eq a => MeetSemiLattice (FiniteSet a) where
    (/\) = intersection

instance Eq a => Lattice (FiniteSet a)

instance Eq a => KeyContainer (FiniteSet a) where
    elementKey a = a
    lookupElement key = List.find (\k -> k == key)
    insertElement e (MkFiniteSet []) = MkFiniteSet [e]
    insertElement e (MkFiniteSet (a:aa))
        | e == a = MkFiniteSet $ e : aa
    insertElement e (MkFiniteSet (a:aa)) = MkFiniteSet $ a : (unFiniteSet $ insertElement e $ MkFiniteSet aa)
    deleteElement _ (MkFiniteSet []) = MkFiniteSet []
    deleteElement k (MkFiniteSet (k':aa))
        | k == k' = MkFiniteSet $ aa
    deleteElement k (MkFiniteSet (a:aa)) = MkFiniteSet $ a : (unFiniteSet $ deleteElement k $ MkFiniteSet aa)
    fromElementList = MkFiniteSet

instance (Eq key, Random key) => IONewItemKeyContainer (FiniteSet key) where
    newKeyContainerItem = randomIO

maybePoint :: (Monoid l, MonoPointed l, Element l ~ a) => Maybe a -> l
maybePoint Nothing = mempty
maybePoint (Just a) = opoint a
