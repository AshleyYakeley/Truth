module Data.Changes.FloatingLens where
{
	import Data.Changes.Edit;
	import Data.FunctorOne;
	import Data.ConstFunction;
	import Prelude hiding (id,(.));

	data FloatingLens' m state a b = MkFloatingLens
	{
		lensUpdate :: Edit a -> state -> ConstFunction a (state,Maybe (Edit b)),
		lensGet :: state -> a -> b,
		lensPutEdit :: state -> Edit b -> ConstFunction a (m (Edit a))	-- m failure means impossible
	};
	
	type FloatingLens = FloatingLens' Maybe;
	
	toFloatingLens :: (FunctorOne m) => FloatingLens' m state a b -> FloatingLens state a b;
	toFloatingLens lens = MkFloatingLens
	{
		lensUpdate = lensUpdate lens,
		lensGet = lensGet lens,
		lensPutEdit = \state edit -> fmap getMaybeOne (lensPutEdit lens state edit)
	};
}
