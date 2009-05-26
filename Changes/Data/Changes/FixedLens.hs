module Data.Changes.FixedLens where
{
	import Data.Changes.Edit;
	import Data.Bijection;
	import Data.Codec;
	import Data.Result;
	import Data.TypeFunc;
	import Data.ConstFunction;
	import Data.Chain;
	import Data.Traversable;
	import Data.OpenWitness;
	import Data.FunctorOne;
	import Control.Applicative;
	import Control.Category;
	import Prelude hiding (id,(.));

	-- | A FixedLens is a lens without state
	;
	data FixedLens' m a b = MkFixedLens
	{
		fixedLensWitness :: LensWitness a b,
		fixedLensUpdateCF :: Edit a -> ConstFunction a (Maybe (Edit b)),
		fixedLensGet :: a -> b,
--		fixedLensPutback :: b -> ConstFunction a (Maybe a)
		fixedLensPutEdit :: Edit b -> ConstFunction a (m (Edit a))
	};
	
	type FixedLens = FixedLens' Maybe;
	
	makeFixedLensUpdateCF :: (Editable a) => (a -> b) -> (Edit a -> Maybe (ConstFunction a (Maybe (Edit b)))) -> (Edit a -> ConstFunction a (Maybe (Edit b)));
	makeFixedLensUpdateCF getter ff edit = case ff edit of
	{
		Just ameb -> ameb;
		_ -> fmap (Just . ReplaceEdit . getter) (applyEditCF edit);
	};
	
	fixedLensUpdate :: FixedLens' m a b -> a -> Edit a -> Maybe (Edit b);
	fixedLensUpdate lens olda edit = applyConstFunction (fixedLensUpdateCF lens edit) olda;
	
	instance (Applicative m,FunctorOne m) => Category (FixedLens' m) where
	{
		id = MkFixedLens
		{
			fixedLensWitness = id,
			fixedLensUpdateCF = \edit -> pure (Just edit),
			fixedLensGet = id,
			fixedLensPutEdit = \editb -> pure (pure editb)
--			fixedLensPutback = \b -> pure (Just b)
		};
		bc . ab = MkFixedLens
		{
			fixedLensWitness = (fixedLensWitness bc) . (fixedLensWitness ab),
			fixedLensUpdateCF = \edita -> do
			{
				meb <- fixedLensUpdateCF ab edita;
				case meb of
				{
					Just editb -> cofmap1CF (fixedLensGet ab) (fixedLensUpdateCF bc editb);
					_ -> return Nothing;
				};
			},
			fixedLensGet = (fixedLensGet bc) . (fixedLensGet ab),
{-
			fixedLensPutback = \c -> do
			{
				mb <- cofmap1CF (fixedLensGet ab) (fixedLensPutback bc c);
				case mb of
				{
					Just b -> fixedLensPutback ab b;
					_ -> return Nothing;
				}
			}
-}
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
	
	voidStateWitness :: LensWitness a ();
	voidStateWitness = makeLensWitness (unsafeIOWitnessFromString "Data.Changes.Edit.void.state" :: IOWitness (TFConst ()));
	
	fixedFloatingLens :: FixedLens' m a b -> FloatingLens' m () a b;
	fixedFloatingLens lens = MkFloatingLens
	{
		lensWitness = fixedLensWitness lens,
		lensStateWitness = voidStateWitness,
		lensUpdate = \edit _ -> do
		{
			meb <- fixedLensUpdateCF lens edit;
			return ((),meb);
		},
		lensGet = \_ -> fixedLensGet lens,
{-
		lensPutback = \_ b -> do
		{
			ma <- fixedLensPutback lens b;
			return (fmap (\newa -> (newa,())) ma);
		},
-}
		lensPutEdit = \_ -> fixedLensPutEdit lens
	};
	
	data CleanLens' m a b = MkCleanLens
	{
		cleanLensWitness :: LensWitness a b,
		cleanLensUpdateCF :: Edit a -> Maybe (Edit b),
		cleanLensGet :: a -> b,
		cleanLensPutEdit :: Edit b -> m (Edit a)
	};
	
	--type CleanLens = CleanLens' Maybe;
	
	instance (Monad m) => Category (CleanLens' m) where
	{
		id = MkCleanLens
		{
			cleanLensWitness = id,
			cleanLensUpdateCF = Just,
			cleanLensGet = id,
			cleanLensPutEdit = return
		};
		bc . ab = MkCleanLens
		{
			cleanLensWitness = (cleanLensWitness bc) . (cleanLensWitness ab),
			cleanLensUpdateCF = \edita -> do
			{
				editb <- cleanLensUpdateCF ab edita;
				cleanLensUpdateCF bc editb;
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
		fixedLensWitness = cleanLensWitness lens,
		fixedLensUpdateCF = \edit -> pure (cleanLensUpdateCF lens edit),
		fixedLensGet = cleanLensGet lens,
		fixedLensPutEdit = \edit -> pure (cleanLensPutEdit lens edit)
	};
	
--	fixedLensEdit :: FixedLens a b -> Edit b -> Edit a;
--	fixedLensEdit lens edit = StateLensEdit (fixedFloatingLens lens) () edit;
	
	-- | A SimpleLens is a FixedLens that doesn't bother with Edits.
	;
	data SimpleLens' m a b = MkSimpleLens
	{
		simpleLensWitness :: LensWitness a b,
		simpleLensGet :: a -> b,
		simpleLensPutback :: b -> ConstFunction a (m a)
	};
	
	type SimpleLens = SimpleLens' Maybe;
	
	instance (Applicative m,FunctorOne m) => Category (SimpleLens' m) where
	{
		id = MkSimpleLens
		{
			simpleLensWitness = id,
			simpleLensGet = id,
			simpleLensPutback = \b -> pure (pure b)
		};
		bc . ab = MkSimpleLens
		{
			simpleLensWitness = (simpleLensWitness bc) . (simpleLensWitness ab),
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
		fixedLensWitness = simpleLensWitness lens,
		fixedLensUpdateCF = makeFixedLensUpdateCF (simpleLensGet lens) (\_ -> Nothing),
		fixedLensGet = simpleLensGet lens,
--		fixedLensPutback = simpleLensPutback lens
--		fixedLensPutEdit :: Edit b -> ConstFunction a (Maybe (Edit a))
		fixedLensPutEdit = \editb -> do
		{
			newb <- cofmap1CF (simpleLensGet lens) (applyEditCF editb);
			ma <- simpleLensPutback lens newb;
			return (fmap ReplaceEdit ma);
		}
	};
	
	data WholeLens' m a b = MkWholeLens
	{
		wholeLensWitness :: LensWitness a b,
		wholeLensGet :: a -> b,
		wholeLensPutback :: b -> m a
	};
	
	type WholeLens = WholeLens' Maybe;
	
	instance (Applicative m,FunctorOne m) => Category (WholeLens' m) where
	{
		id = MkWholeLens
		{
			wholeLensWitness = id,
			wholeLensGet = id,
			wholeLensPutback = pure
		};
		bc . ab = MkWholeLens
		{
			wholeLensWitness = (wholeLensWitness bc) . (wholeLensWitness ab),
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
		simpleLensWitness = wholeLensWitness lens,
		simpleLensGet = wholeLensGet lens,
		simpleLensPutback = \b -> pure (wholeLensPutback lens b)
	};

	traversableWholeLens :: forall f m a b. (Traversable f,Applicative m) => WholeLens' m a b -> WholeLens' m (f a) (f b);
	traversableWholeLens lens = MkWholeLens
	{
		wholeLensWitness = cfmap (wholeLensWitness lens),
		wholeLensGet = fmap (wholeLensGet lens),
		wholeLensPutback = putback
	}
	where
	{
		putback fb = sequenceA (fmap (wholeLensPutback lens) fb);
	};
	
	resultWholeLens :: LensWitness a (Result e b) -> (a -> Result e b) -> (b -> a) -> WholeLens' Maybe a (Result e b);
	resultWholeLens witness decode' encode' = MkWholeLens
	{
		wholeLensWitness = witness,
		wholeLensGet = decode',
		wholeLensPutback = \r -> case r of
		{
			SuccessResult b -> Just (encode' b);
			_ -> Nothing;
		}
	};
	
	codecWholeLens :: LensWitness a (Maybe b) -> Codec a b -> WholeLens' Maybe a (Maybe b);
	codecWholeLens wit codec = MkWholeLens
	{
		wholeLensWitness = wit,
		wholeLensGet = decode codec,
		wholeLensPutback = fmap (encode codec)
	};
	
	bijectionWholeLens :: (Applicative m) => LensWitness a b -> Bijection a b -> WholeLens' m a b;
	bijectionWholeLens wit bi = MkWholeLens
	{
		wholeLensWitness = wit,
		wholeLensGet = biForwards bi,
		wholeLensPutback = pure . (biBackwards bi)
	};
}
