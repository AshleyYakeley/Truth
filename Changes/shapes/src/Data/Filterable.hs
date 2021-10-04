module Data.Filterable where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as List
import Shapes.Import

class Functor f => Filterable f where
    mapMaybe :: (a -> Maybe b) -> f a -> f b
    catMaybes :: f (Maybe a) -> f a
    catMaybes = mapMaybe id
    filter :: (a -> Bool) -> f a -> f a
    filter test =
        mapMaybe $ \a ->
            if test a
                then Just a
                else Nothing

instance Filterable [] where
    mapMaybe = List.mapMaybe
    catMaybes = List.catMaybes
    filter = List.filter

instance Filterable Maybe where
    mapMaybe amb ma = ma >>= amb

instance Filterable (Map k) where
    mapMaybe = Map.mapMaybe

forf :: (Applicative m, Filterable f, Traversable f) => f a -> (a -> m (Maybe b)) -> m (f b)
forf fa ammb = fmap catMaybes $ for fa ammb

forfilt :: (Applicative m, Filterable f, Traversable f) => f a -> (a -> m Bool) -> m (f a)
forfilt fa amt =
    forf fa $ \a ->
        fmap
            (\t ->
                 if t
                     then Just a
                     else Nothing)
            (amt a)
