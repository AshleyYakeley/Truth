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
	import Control.Applicative;
	import Control.Category;
	import Prelude hiding (id,(.));


	data FixedLens a b = MkFixedLens
	{
		fixedLensWitness :: LensWitness a b,
		fixedLensUpdateCF :: Edit a -> ConstFunction a (Maybe (Edit b)),
		fixedLensGet :: a -> b,
		fixedLensPutback :: b -> ConstFunction a (Maybe a)
	};
	
	makeFixedLensUpdateCF :: (a -> b) -> (Edit a -> Maybe (ConstFunction a (Maybe (Edit b)))) -> (Edit a -> ConstFunction a (Maybe (Edit b)));
	makeFixedLensUpdateCF getter ff edit = case ff edit of
	{
		Just ameb -> ameb;
		_ -> fmap (Just . ReplaceEdit . getter) (applyEditCF edit);
	};
	
	fixedLensUpdate :: FixedLens a b -> a -> Edit a -> Maybe (Edit b);
	fixedLensUpdate lens olda edit = applyConstFunction (fixedLensUpdateCF lens edit) olda;
	
	instance Category FixedLens where
	{
		id = MkFixedLens
		{
			fixedLensWitness = id,
			fixedLensUpdateCF = \edit -> pure (Just edit),
			fixedLensGet = id,
			fixedLensPutback = \b -> pure (Just b)
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
			fixedLensPutback = \c -> do
			{
				mb <- cofmap1CF (fixedLensGet ab) (fixedLensPutback bc c);
				case mb of
				{
					Just b -> fixedLensPutback ab b;
					_ -> return Nothing;
				}
			}
		};
	};
	
	voidStateWitness :: LensWitness a ();
	voidStateWitness = makeLensWitness (unsafeIOWitnessFromString "Data.Changes.Edit.void.state" :: IOWitness (TFConst ()));
	
	fixedFloatingLens :: FixedLens a b -> FloatingLens () a b;
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
		lensPutback = \_ b -> do
		{
			ma <- fixedLensPutback lens b;
			return (fmap (\newa -> (newa,())) ma);
		}		
	};
	
	fixedLensEdit :: FixedLens a b -> Edit b -> Edit a;
	fixedLensEdit lens edit = StateLensEdit (fixedFloatingLens lens) () edit;
	
	data SimpleLens a b = MkSimpleLens
	{
		simpleLensWitness :: LensWitness a b,
		simpleLensGet :: a -> b,
		simpleLensPutback :: b -> ConstFunction a (Maybe a)
	};
	
	instance Category SimpleLens where
	{
		id = MkSimpleLens
		{
			simpleLensWitness = id,
			simpleLensGet = id,
			simpleLensPutback = \b -> pure (Just b)
		};
		bc . ab = MkSimpleLens
		{
			simpleLensWitness = (simpleLensWitness bc) . (simpleLensWitness ab),
			simpleLensGet = (simpleLensGet bc) . (simpleLensGet ab),
			simpleLensPutback = \c -> do
			{
				mb <- cofmap1CF (simpleLensGet ab) (simpleLensPutback bc c);
				case mb of
				{
					Just b -> simpleLensPutback ab b;
					_ -> return Nothing;
				}
			}
		};
	};
	
	simpleFixedLens :: SimpleLens a b -> FixedLens a b;
	simpleFixedLens lens = MkFixedLens
	{
		fixedLensWitness = simpleLensWitness lens,
		fixedLensUpdateCF = makeFixedLensUpdateCF (simpleLensGet lens) (\_ -> Nothing),
		fixedLensGet = simpleLensGet lens,
		fixedLensPutback = simpleLensPutback lens
	};
	
	data WholeLens a b = MkWholeLens
	{
		wholeLensWitness :: LensWitness a b,
		wholeLensGet :: a -> b,
		wholeLensPutback :: b -> Maybe a
	};
	
	instance Category WholeLens where
	{
		id = MkWholeLens
		{
			wholeLensWitness = id,
			wholeLensGet = id,
			wholeLensPutback = Just
		};
		bc . ab = MkWholeLens
		{
			wholeLensWitness = (wholeLensWitness bc) . (wholeLensWitness ab),
			wholeLensGet = (wholeLensGet bc) . (wholeLensGet ab),
			wholeLensPutback = \c -> (wholeLensPutback bc c) >>= (wholeLensPutback ab)
		};
	};
	
	wholeSimpleLens :: WholeLens a b -> SimpleLens a b;
	wholeSimpleLens lens = MkSimpleLens
	{
		simpleLensWitness = wholeLensWitness lens,
		simpleLensGet = wholeLensGet lens,
		simpleLensPutback = \b -> pure (wholeLensPutback lens b)
	};

	traversableWholeLens :: forall f a b. (Traversable f) => WholeLens a b -> WholeLens (f a) (f b);
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
	
	resultWholeLens :: LensWitness a (Result e b) -> (a -> Result e b) -> (b -> a) -> WholeLens a (Result e b);
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
	
	codecWholeLens :: LensWitness a (Maybe b) -> Codec a b -> WholeLens a (Maybe b);
	codecWholeLens wit codec = MkWholeLens
	{
		wholeLensWitness = wit,
		wholeLensGet = decode codec,
		wholeLensPutback = fmap (encode codec)
	};
	
	bijectionWholeLens :: LensWitness a b -> Bijection a b -> WholeLens a b;
	bijectionWholeLens wit bi = MkWholeLens
	{
		wholeLensWitness = wit,
		wholeLensGet = biForwards bi,
		wholeLensPutback = Just . (biBackwards bi)
	};
}
