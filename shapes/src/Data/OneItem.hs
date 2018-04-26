module Data.OneItem where

import Shapes.Import

data OneItem a
    = OneItemNone
    | OneItemOne a
    | OneItemMore

instance Semigroup (OneItem a) where
    OneItemNone <> b = b
    a <> OneItemNone = a
    _ <> _ = OneItemMore

instance Monoid (OneItem a) where
    mempty = OneItemNone
    mappend = (<>)

instance Functor OneItem where
    fmap _ OneItemNone = OneItemNone
    fmap ab (OneItemOne a) = OneItemOne $ ab a
    fmap _ OneItemMore = OneItemMore

oneItemOne :: OneItem a -> Maybe a
oneItemOne (OneItemOne a) = Just a
oneItemOne _ = Nothing

getOneItem :: MonoFoldable mono => mono -> OneItem (Element mono)
getOneItem = ofoldMap OneItemOne

getSingle :: MonoFoldable mono => mono -> Maybe (Element mono)
getSingle = oneItemOne . getOneItem
