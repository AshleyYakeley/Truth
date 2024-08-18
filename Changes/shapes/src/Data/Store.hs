module Data.Store
    ( module Data.Store
    , Key
    ) where

import Shapes.Import

data Store a =
    MkStore Key
            (IntMap a)

instance Functor Store where
    fmap ab (MkStore key mp) = MkStore key $ fmap ab mp

instance Foldable Store where
    foldMap am (MkStore _ mp) = concatmap am $ toList mp

instance Traversable Store where
    traverse amb (MkStore i mp) = MkStore i <$> traverse amb mp

emptyStore :: Store a
emptyStore = MkStore 0 mempty

isEmptyStore :: Store a -> Bool
isEmptyStore (MkStore _ mp) = null mp

addStore :: a -> Store a -> (Key, Store a)
addStore a (MkStore i mp) = (i, MkStore (succ i) (insertMap i a mp))

lookupStore :: Key -> Store a -> Maybe a
lookupStore key (MkStore _ mp) = lookup key mp

deleteStore :: Key -> Store a -> Store a
deleteStore i (MkStore n mp) = MkStore n (deleteMap i mp)

allStoreExcept :: Store a -> Key -> [a]
allStoreExcept (MkStore _ mp) i = toList (deleteMap i mp)

traverseStore :: Applicative m => (Key -> a -> m b) -> Store a -> m (Store b)
traverseStore kamb (MkStore i mp) = MkStore i <$> traverseWithKey kamb mp

addStoreStateT :: Applicative m => a -> StateT (Store a) m Key
addStoreStateT a = StateT $ \oldstore -> pure $ addStore a oldstore

lookupStoreStateT :: Applicative m => Key -> StateT (Store a) m (Maybe a)
lookupStoreStateT key = StateT $ \store -> pure (lookupStore key store, store)

deleteStoreStateT :: Applicative m => Key -> StateT (Store a) m ()
deleteStoreStateT key = StateT $ \oldstore -> pure ((), deleteStore key oldstore)

traverseStoreStateT :: Applicative m => (Key -> StateT s m ()) -> StateT (Store s) m ()
traverseStoreStateT f =
    StateT $ \oldstore ->
        fmap (\newstore -> ((), newstore)) $
        traverseStore (\key oldstate -> fmap snd $ runStateT (f key) oldstate) oldstore
