module Data.Store (module Data.Store,Key) where
{
    import Prelude hiding (null,lookup);
    import Data.IntMap;

    data Store a = MkStore Key (IntMap a);

    instance Functor Store where
    {
        fmap ab (MkStore key mp) = MkStore key $ fmap ab mp;
    };

    instance Foldable Store where
    {
        foldMap am (MkStore _ mp) = mconcat $ fmap am $ elems mp;
    };

    instance Traversable Store where
    {
        traverse amb (MkStore i mp) = MkStore i <$> traverse amb mp;
    };

    emptyStore :: Store a;
    emptyStore = MkStore 0 empty;

    isEmptyStore :: Store a -> Bool;
    isEmptyStore (MkStore _ mp) = null mp;

    addStore :: a -> Store a -> (Key,Store a);
    addStore a (MkStore i mp) = (i,MkStore (i+1) (insert i a mp));

    lookupStore :: Key -> Store a -> Maybe a;
    lookupStore key (MkStore _ mp) = lookup key mp;

    deleteStore :: Key -> Store a -> Store a;
    deleteStore i (MkStore n mp) = MkStore n (delete i mp);

    allStoreExcept :: Store a -> Key -> [a];
    allStoreExcept (MkStore _ mp) i = elems (delete i mp);

    traverseStore :: Applicative m => (Key -> a -> m b) -> Store a -> m (Store b);
    traverseStore kamb (MkStore i mp) = MkStore i <$> traverseWithKey kamb mp;
}
