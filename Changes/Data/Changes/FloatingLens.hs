module Data.Changes.FloatingLens where
{
	import Data.Changes.Edit;
	import Data.FunctorOne;
	import Data.ConstFunction;
	import Data.Result;
	import Data.Chain;
	import Control.Applicative;

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

	-- suitable for Results, trying to put a failure code will be rejected 
	resultLens :: (FunctorOne f,PartEdit (f a) ~ JustEdit a,PartEdit (f b) ~ JustEdit b) => FloatingLens state a b -> FloatingLens state (f a) (f b);
	resultLens lens  = MkFloatingLens
	{
		lensUpdate = \editfa state -> case extractJustEdit editfa of
		{
			Just edita -> do
			{
				msmeb <-  cofmap1CF getMaybeOne (cfmap (lensUpdate lens edita state));
				return (case msmeb of
				{
					Just (newstate,meditb) -> (newstate,fmap (PartEdit . JustEdit) meditb);
					Nothing -> (state,Nothing);
				});
			};
			Nothing -> pure (state,Nothing);
		},
		lensGet = \state -> fmap (lensGet lens state),
		lensPutEdit = \state editfb -> case extractJustEdit editfb of
		{
			Just editb -> do
			{
				mmea <- cofmap1CF getMaybeOne (cfmap (lensPutEdit lens state editb));
				return (case mmea of
				{
					Just (Just edita) -> Just (PartEdit (JustEdit edita));
					_ -> Nothing;
				});
			};
			Nothing -> pure Nothing;
		}
	};
	
	instance CatFunctor (FloatingLens state) (Result err) where
	{
		cfmap = resultLens;
	};
	
	instance CatFunctor (FloatingLens state) Maybe where
	{
		cfmap = resultLens;
	};
}
