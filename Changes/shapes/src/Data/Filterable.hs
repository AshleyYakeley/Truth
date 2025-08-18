module Data.Filterable where

import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe qualified as List
import Data.Sequences qualified

import Data.Codec
import Shapes.Import

class Invariant f => InjectiveFilterable f where
    injectiveFilter :: forall a b. Codec a b -> f a -> f b
    default injectiveFilter :: forall a b. Filterable f => Codec a b -> f a -> f b
    injectiveFilter codec = mapMaybe $ decode codec
    catMaybes :: f (Maybe a) -> f a
    catMaybes = injectiveFilter justCodec
    filter :: (a -> Bool) -> f a -> f a
    filter test = injectiveFilter $ ifCodec test

instance InjectiveFilterable (Codec p) where
    injectiveFilter = (.)

injectiveFilter' :: (InjectiveFilterable f, MonadInner m) => Codec' m a b -> f a -> f b
injectiveFilter' codec = injectiveFilter $ toCodec codec

class (Functor f, InjectiveFilterable f) => Filterable f where
    mapMaybe :: (a -> Maybe b) -> f a -> f b

instance InjectiveFilterable [] where
    catMaybes = List.catMaybes
    filter = List.filter

instance Filterable [] where
    mapMaybe = List.mapMaybe

instance InjectiveFilterable Maybe

instance Filterable Maybe where
    mapMaybe amb ma = ma >>= amb

instance InjectiveFilterable (Map k)

instance Filterable (Map k) where
    mapMaybe = Map.mapMaybe

forf :: (Applicative m, InjectiveFilterable f, Traversable f) => f a -> (a -> m (Maybe b)) -> m (f b)
forf fa ammb = fmap catMaybes $ for fa ammb

filterM :: (Applicative m, InjectiveFilterable f, Traversable f) => (a -> m Bool) -> f a -> m (f a)
filterM amt fa =
    forf fa $ \a ->
        fmap
            ( \t ->
                if t
                    then Just a
                    else Nothing
            )
            (amt a)

forfilt :: (Applicative m, InjectiveFilterable f, Traversable f) => f a -> (a -> m Bool) -> m (f a)
forfilt fa amt = filterM amt fa

class MonoFoldable c => MonoFilterable c where
    ofilter :: (Element c -> Bool) -> c -> c
    default ofilter :: (c ~ f a, InjectiveFilterable f, Element c ~ a) => (Element c -> Bool) -> c -> c
    ofilter = filter
    ofilterM ::
        forall m.
        Applicative m =>
        (Element c -> m Bool) ->
        c ->
        m c
    default ofilterM ::
        (Applicative m, c ~ f a, InjectiveFilterable f, Traversable f, Element c ~ a) =>
        (Element c -> m Bool) -> c -> m c
    ofilterM = filterM

oforfilt ::
    forall c m.
    (MonoFilterable c, Applicative m) =>
    c ->
    (Element c -> m Bool) ->
    m c
oforfilt fa amt = ofilterM amt fa

instance MonoFilterable [a]

instance MonoFilterable (Maybe a)

instance MonoFilterable (Map k a)

instance MonoFilterable StrictByteString where
    ofilter = Data.Sequences.filter
    ofilterM f = liftA fromList . ofilterM f . otoList

instance MonoFilterable Text where
    ofilter = Data.Sequences.filter
    ofilterM f = liftA fromList . ofilterM f . otoList
