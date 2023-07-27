module Data.Merge where

import Shapes.Import

type Merge a = a -> a -> Maybe a

eqMerge :: Eq a => Merge a
eqMerge a b
    | a == b = Just a
eqMerge _ _ = Nothing

eqMergeOn :: Eq a => (b -> a) -> Merge b
eqMergeOn ba b1 b2
    | ba b1 == ba b2 = Just b1
eqMergeOn _ _ _ = Nothing

semigroupMerge :: Semigroup a => Merge a
semigroupMerge a b = Just $ a <> b

mergeItemList :: Merge a -> a -> [a] -> (a, [a])
mergeItemList _m a [] = (a, [])
mergeItemList m a (i:ii) =
    case m a i of
        Just ai -> mergeItemList m ai ii
        Nothing -> fmap ((:) i) $ mergeItemList m a ii

mergeList :: Merge a -> [a] -> [a]
mergeList _ [] = []
mergeList m (a:aa) = let
    (a', aa') = mergeItemList m a $ mergeList m aa
    in a' : aa'
