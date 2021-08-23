module Data.KeyedOrder
    ( OrderedList
    , olEmpty
    , olFromList
    , olLength
    , olGetByPos
    , olLookupByPredicate
    , olLookupByItem
    , olInsert
    , olDeleteByPos
    ) where

import Data.Sequence (Seq, ViewL(..), (<|), viewl)
import Shapes.Import

data OrderedList a =
    MkOrderedList (a -> a -> Ordering)
                  (Seq a)

instance Foldable OrderedList where
    foldMap am (MkOrderedList _ items) = foldMap am items

olEmpty :: (a -> a -> Ordering) -> OrderedList a
olEmpty cmp = MkOrderedList cmp mempty

olFromList :: (a -> a -> Ordering) -> [a] -> OrderedList a
olFromList cmp aa = MkOrderedList cmp $ fromList $ sortBy cmp aa

olLength :: OrderedList a -> Int
olLength (MkOrderedList _ items) = length items

olGetByPos :: OrderedList a -> Int -> Maybe a
olGetByPos _ i
    | i < 0 = Nothing
olGetByPos (MkOrderedList _ items) i
    | i >= length items = Nothing
olGetByPos (MkOrderedList _ items) i = index items i

olLookupByPredicate :: OrderedList a -> (a -> Bool) -> Maybe (a, Int)
olLookupByPredicate (MkOrderedList _ items) p = let
    findItem ii =
        case viewl ii of
            EmptyL -> Nothing
            item :< _
                | p item -> Just (item, 0)
            _ :< r -> do
                (item, i) <- findItem r
                return (item, succ i)
    in findItem items

olInsert :: a -> OrderedList a -> (Int, OrderedList a)
olInsert a ol@(MkOrderedList cmp items) = let
    (found, pos) = olLookupByItem ol a
    in if found
           then (pos, ol)
           else let
                    (before, after) = splitAt pos items
                    in (pos, MkOrderedList cmp $ before <> (a <| after))

olDeleteByPos :: Int -> OrderedList a -> OrderedList a
olDeleteByPos i (MkOrderedList cmp items) =
    MkOrderedList cmp $ let
        (before, after) = splitAt i items
        in before <> drop 1 after

olLookupByItem :: OrderedList a -> a -> (Bool, Int)
olLookupByItem (MkOrderedList cmp items) k = let
    -- TODO: improve inefficient linear search
    findItem ii =
        case viewl ii of
            EmptyL -> (False, 0)
            item :< r ->
                case cmp k item of
                    LT -> (False, 0)
                    EQ -> (True, 0)
                    GT -> let
                        (found, t) = findItem r
                        in (found, succ t)
    in findItem items
