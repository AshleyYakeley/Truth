module Data.Store where
{
	import Data.IntMap;
	import Prelude hiding (null);

	data Store a = MkStore Int (IntMap a);

	emptyStore :: Store a;
	emptyStore = MkStore 0 empty;
	
	isEmptyStore :: Store a -> Bool;
	isEmptyStore (MkStore _ mp) = null mp;
	
	addStore :: a -> Store a -> (Int,Store a);
	addStore a (MkStore i mp) = (i,MkStore (i+1) (insert i a mp));

	lookupStore :: Store a -> Int -> a;
	lookupStore (MkStore _ mp) i = mp ! i;
	
	deleteStore :: Int -> Store a -> Store a;
	deleteStore i (MkStore n mp) = MkStore n (delete i mp);
	
	allStore :: Store a -> [a];
	allStore (MkStore _ mp) = elems mp;
}
