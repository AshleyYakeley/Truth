module Data.Changes.Tuple where
{
	import Data.Changes.Edit;
	import Data.Witness;
--	import Data.OpenWitness;
--	import Control.Monad;
--	import Data.Maybe;
--	import Data.TypeFunc;
	import Prelude;

	
	{-
	data SimpleLens a b = MkSimpleLens
	{
		simpleLensWitness :: LensWitness a b,
		simpleLensGet :: a -> b,
		simpleLensPutback :: b -> a -> Maybe a
	};
	-}

	class (Is (ListType Type) t) => IsTuple t a | a -> t where
	{
		fromListTuple :: t -> a;
		toListTuple :: a -> t;
	};

	instance IsTuple (a,(b,(c,()))) (a,b,c) where
	{
		fromListTuple (a,(b,(c,()))) = (a,b,c);
		toListTuple (a,b,c) = (a,(b,(c,())));
	};

	fstSimpleLens :: IsTuple (first,rest) a => LensWitness a first -> SimpleLens a first;
	fstSimpleLens witness = MkSimpleLens
	{
		simpleLensWitness = witness,
		simpleLensGet = fst . toListTuple,
		simpleLensPutback = \first a -> Just (fromListTuple (first,snd (toListTuple a)))
	};

}
