module Data.Store where
{
	import Data.IntMap;
	import Prelude hiding (null);

	data Store a = MkStore Key (IntMap a);

	emptyStore :: Store a;
	emptyStore = MkStore 0 empty;
	
	isEmptyStore :: Store a -> Bool;
	isEmptyStore (MkStore _ mp) = null mp;
	
	addStore :: a -> Store a -> (Key,Store a);
	addStore a (MkStore i mp) = (i,MkStore (i+1) (insert i a mp));

	lookupStore :: Store a -> Key -> a;
	lookupStore (MkStore _ mp) i = mp ! i;
	
	deleteStore :: Key -> Store a -> Store a;
	deleteStore i (MkStore n mp) = MkStore n (delete i mp);
	
	allStore :: Store a -> [a];
	allStore (MkStore _ mp) = elems mp;
	
	allStoreExcept :: Store a -> Key -> [a];
	allStoreExcept (MkStore _ mp) i = elems (delete i mp);
}
