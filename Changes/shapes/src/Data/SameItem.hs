module Data.SameItem where

import Shapes.Import

data SameItem a
    = SameItemNone
    | SameItemOne a
    | SameItemMore

instance Eq a => Semigroup (SameItem a) where
    SameItemNone <> b = b
    a <> SameItemNone = a
    oa@(SameItemOne a) <> SameItemOne b
        | a == b = oa
    _ <> _ = SameItemMore

instance Eq a => Monoid (SameItem a) where
    mempty = SameItemNone
    mappend = (<>)

instance Functor SameItem where
    fmap _ SameItemNone = SameItemNone
    fmap ab (SameItemOne a) = SameItemOne $ ab a
    fmap _ SameItemMore = SameItemMore

oneItemOne :: SameItem a -> Maybe a
oneItemOne (SameItemOne a) = Just a
oneItemOne _ = Nothing

getSameItem :: Eq (Element mono) => MonoFoldable mono => mono -> SameItem (Element mono)
getSameItem = ofoldMap SameItemOne

getSingle :: Eq (Element mono) => MonoFoldable mono => mono -> Maybe (Element mono)
getSingle = oneItemOne . getSameItem
