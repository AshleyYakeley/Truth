module Data.Changes.FixedLens where
{
	import Data.Changes.Edit;
	import Data.Bijection;
	import Data.Codec;
	import Data.Result;
	import Data.ConstFunction;
	import Data.Traversable;
	import Data.FunctorOne;
	import Control.Applicative;
	import Control.Category;
	import Prelude hiding (id,(.));

	-- | A FixedLens is a lens without state
	;
	data FixedLens' m a b = MkFixedLens
	{
		fixedLensUpdate :: Edit a -> ConstFunction a (Maybe (Edit b)),
		fixedLensGet :: a -> b,
		fixedLensPutEdit :: Edit b -> ConstFunction a (m (Edit a))
	};
	
	type FixedLens = FixedLens' Maybe;
	
	makeFixedLensUpdate :: (Editable a) => (a -> b) -> (Edit a -> Maybe (ConstFunction a (Maybe (Edit b)))) -> (Edit a -> ConstFunction a (Maybe (Edit b)));
	makeFixedLensUpdate getter ff edit = case ff edit of
	{
		Just ameb -> ameb;
		_ -> fmap (Just . ReplaceEdit . getter) (applyEdit edit);
	};
	
	instance (Applicative m,FunctorOne m) => Category (FixedLens' m) where
	{
		id = MkFixedLens
		{
			fixedLensUpdate = \edit -> pure (Just edit),
			fixedLensGet = id,
			fixedLensPutEdit = \editb -> pure (pure editb)
		};
		bc . ab = MkFixedLens
		{
			fixedLensUpdate = \edita -> do
			{
				meb <- fixedLensUpdate ab edita;
				case meb of
				{
					Just editb -> cofmap1CF (fixedLensGet ab) (fixedLensUpdate bc editb);
					_ -> return Nothing;
				};
			},
			fixedLensGet = (fixedLensGet bc) . (fixedLensGet ab),
			fixedLensPutEdit = \editc -> do
			{
				meditb <- cofmap1CF (fixedLensGet ab) (fixedLensPutEdit bc editc);
				case retrieveOne meditb of
				{
					SuccessResult editb -> fixedLensPutEdit ab editb;
					FailureResult ff -> return ff;
				};
			}
		};
	};
	
	fixedFloatingLens :: FixedLens' m a b -> FloatingLens' m () a b;
	fixedFloatingLens lens = MkFloatingLens
	{
		lensUpdate = \edit _ -> do
		{
			meb <- fixedLensUpdate lens edit;
			return ((),meb);
		},
		lensGet = \_ -> fixedLensGet lens,
		lensPutEdit = \_ -> fixedLensPutEdit lens
	};
	
	data CleanLens' m a b = MkCleanLens
	{
		cleanLensUpdate :: Edit a -> Maybe (Edit b),
		cleanLensGet :: a -> b,
		cleanLensPutEdit :: Edit b -> m (Edit a)
	};
	
	--type CleanLens = CleanLens' Maybe;
	
	instance (Monad m) => Category (CleanLens' m) where
	{
		id = MkCleanLens
		{
			cleanLensUpdate = Just,
			cleanLensGet = id,
			cleanLensPutEdit = return
		};
		bc . ab = MkCleanLens
		{
			cleanLensUpdate = \edita -> do
			{
				editb <- cleanLensUpdate ab edita;
				cleanLensUpdate bc editb;
			},
			cleanLensGet = (cleanLensGet bc) . (cleanLensGet ab),

			cleanLensPutEdit = \editc -> do
			{
				editb <- cleanLensPutEdit bc editc;
				cleanLensPutEdit ab editb;
			}
		};
	};
	
	cleanFixedLens :: CleanLens' m a b -> FixedLens' m a b;
	cleanFixedLens lens = MkFixedLens
	{
		fixedLensUpdate = \edit -> pure (cleanLensUpdate lens edit),
		fixedLensGet = cleanLensGet lens,
		fixedLensPutEdit = \edit -> pure (cleanLensPutEdit lens edit)
	};
	
	-- | A SimpleLens is a FixedLens that doesn't bother with Edits.
	;
	data SimpleLens' m a b = MkSimpleLens
	{
		simpleLensGet :: a -> b,
		simpleLensPutback :: b -> ConstFunction a (m a)
	};
	
	type SimpleLens = SimpleLens' Maybe;
	
	instance (Applicative m,FunctorOne m) => Category (SimpleLens' m) where
	{
		id = MkSimpleLens
		{
			simpleLensGet = id,
			simpleLensPutback = \b -> pure (pure b)
		};
		bc . ab = MkSimpleLens
		{
			simpleLensGet = (simpleLensGet bc) . (simpleLensGet ab),
			simpleLensPutback = \c -> do
			{
				mb <- cofmap1CF (simpleLensGet ab) (simpleLensPutback bc c);
				case retrieveOne mb of
				{
					SuccessResult b -> simpleLensPutback ab b;
					FailureResult ff -> return ff;
				}
			}
		};
	};
	
	simpleFixedLens :: (Functor m,Editable a,Editable b) => SimpleLens' m a b -> FixedLens' m a b;
	simpleFixedLens lens = MkFixedLens
	{
		fixedLensUpdate = makeFixedLensUpdate (simpleLensGet lens) (\_ -> Nothing),
		fixedLensGet = simpleLensGet lens,
		fixedLensPutEdit = \editb -> do
		{
			newb <- cofmap1CF (simpleLensGet lens) (applyEdit editb);
			ma <- simpleLensPutback lens newb;
			return (fmap ReplaceEdit ma);
		}
	};
	
	data WholeLens' m a b = MkWholeLens
	{
		wholeLensGet :: a -> b,
		wholeLensPutback :: b -> m a
	};
	
	type WholeLens = WholeLens' Maybe;
	
	instance (Applicative m,FunctorOne m) => Category (WholeLens' m) where
	{
		id = MkWholeLens
		{
			wholeLensGet = id,
			wholeLensPutback = pure
		};
		bc . ab = MkWholeLens
		{
			wholeLensGet = (wholeLensGet bc) . (wholeLensGet ab),
			wholeLensPutback = \c -> case retrieveOne (wholeLensPutback bc c) of
			{
				SuccessResult b -> wholeLensPutback ab b;
				FailureResult ff -> ff;
			}
		};
	};
	
	wholeSimpleLens :: WholeLens' m a b -> SimpleLens' m a b;
	wholeSimpleLens lens = MkSimpleLens
	{
		simpleLensGet = wholeLensGet lens,
		simpleLensPutback = \b -> pure (wholeLensPutback lens b)
	};

	traversableWholeLens :: forall f m a b. (Traversable f,Applicative m) => WholeLens' m a b -> WholeLens' m (f a) (f b);
	traversableWholeLens lens = MkWholeLens
	{
		wholeLensGet = fmap (wholeLensGet lens),
		wholeLensPutback = putback
	}
	where
	{
		putback fb = sequenceA (fmap (wholeLensPutback lens) fb);
	};
	
	resultWholeLens :: (a -> Result e b) -> (b -> a) -> WholeLens' Maybe a (Result e b);
	resultWholeLens decode' encode' = MkWholeLens
	{
		wholeLensGet = decode',
		wholeLensPutback = \r -> case r of
		{
			SuccessResult b -> Just (encode' b);
			_ -> Nothing;
		}
	};
	
	codecWholeLens :: Codec a b -> WholeLens' Maybe a (Maybe b);
	codecWholeLens codec = MkWholeLens
	{
		wholeLensGet = decode codec,
		wholeLensPutback = fmap (encode codec)
	};
	
	bijectionWholeLens :: (Applicative m) => Bijection a b -> WholeLens' m a b;
	bijectionWholeLens bi = MkWholeLens
	{
		wholeLensGet = biForwards bi,
		wholeLensPutback = pure . (biBackwards bi)
	};
}
