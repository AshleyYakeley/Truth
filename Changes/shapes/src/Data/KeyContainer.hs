module Data.KeyContainer where

import Data.List qualified

import Data.HasNewValue
import Shapes.Import

class (Monoid t, Semigroup t, MonoFoldable t, GrowingAppend t) => ItemContainer t where
    -- | Not the same as Element.
    -- For a map, `Element m = MapValue m`
    -- but `Item m = (ContainerKey m, MapValue m)`
    type Item t :: Type

    itemElement :: Item t -> Element t
    type Item t = Element t
    default itemElement :: Item t ~ Element t => Item t -> Element t
    itemElement e = e

instance ItemContainer [a]

instance Ord k => ItemContainer (Map k v) where
    type Item (Map k v) = (k, v)
    itemElement (_, v) = v

class (ItemContainer t, SetContainer t) => KeyContainer t where
    itemKey :: Item t -> ContainerKey t
    lookupItem :: ContainerKey t -> t -> Maybe (Item t)
    insertItem :: Item t -> t -> t
    deleteKey :: ContainerKey t -> t -> t
    fromItemList :: [Item t] -> t
    fromItemList = foldr insertItem mempty
    default itemKey :: Item t ~ (ContainerKey t, Element t) => Item t -> ContainerKey t
    itemKey (k, _) = k
    default lookupItem :: (IsMap t, Item t ~ (ContainerKey t, MapValue t)) => ContainerKey t -> t -> Maybe (Item t)
    lookupItem k m = do
        e <- lookup k m
        return (k, e)
    default insertItem :: (IsMap t, Item t ~ (ContainerKey t, MapValue t)) => Item t -> t -> t
    insertItem (k, v) = insertMap k v
    default deleteKey :: IsMap t => ContainerKey t -> t -> t
    deleteKey = deleteMap

instance Eq key => KeyContainer [(key, value)] where
    itemKey (key, _) = key
    lookupItem key = Data.List.find (\(k, _) -> k == key)
    insertItem e [] = [e]
    insertItem e@(k, _) ((k', _) : aa)
        | k == k' = e : aa
    insertItem e (a : aa) = a : (insertItem e aa)
    deleteKey _ [] = []
    deleteKey k ((k', _) : aa)
        | k == k' = aa
    deleteKey k (a : aa) = a : (deleteKey k aa)
    fromItemList = id

instance Ord k => KeyContainer (Map k v)

class KeyContainer t => IONewItemKeyContainer t where
    newKeyContainerItem :: IO (Element t)

instance (Eq key, Random key, HasNewValue value) => IONewItemKeyContainer [(key, value)] where
    newKeyContainerItem = do
        key <- randomIO
        return (key, newValue)
