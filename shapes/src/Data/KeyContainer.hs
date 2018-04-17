module Data.KeyContainer where

import Data.HasNewValue
import qualified Data.List
import Shapes.Import

class (MonoTraversable t, SetContainer t) => KeyContainer t where
    elementKey :: Element t -> ContainerKey t
    lookupElement :: ContainerKey t -> t -> Maybe (Element t)
    insertElement :: Element t -> t -> t
    deleteElement :: ContainerKey t -> t -> t
    fromElementList :: [Element t] -> t
    fromElementList = foldr insertElement mempty

instance Eq key => KeyContainer [(key, value)] where
    elementKey (key, _) = key
    lookupElement key = Data.List.find (\(k, _) -> k == key)
    insertElement e [] = [e]
    insertElement e@(k, _) ((k', _):aa)
        | k == k' = e : aa
    insertElement e (a:aa) = a : (insertElement e aa)
    deleteElement _ [] = []
    deleteElement k ((k', _):aa)
        | k == k' = aa
    deleteElement k (a:aa) = a : (deleteElement k aa)
    fromElementList = id

class KeyContainer t => IONewItemKeyContainer t where
    newKeyContainerItem :: IO (Element t)

instance (Eq key, Random key, HasNewValue value) => IONewItemKeyContainer [(key, value)] where
    newKeyContainerItem = do
        key <- randomIO
        return (key, newValue)
