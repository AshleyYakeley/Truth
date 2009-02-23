module Data.Changes.Edit where
{
	import Data.Bijection;
	import Data.Codec;
	import Data.Result;
	import Data.TypeFunc;
	import Data.FunctorOne;
	import Data.Traversable;
	import Data.OpenWitness;
	import Data.Witness;
	import Control.Category;
	import Prelude hiding (id,(.));

	type LensWitness = TFWitness (TFComposite IOWitness);
	makeLensWitness :: IOWitness tf -> LensWitness x (TF tf x);
	makeLensWitness wit = MkTFWitness (ConstTFComposite wit);

	data Edit a where
	{
		ReplaceEdit :: a -> Edit a;
		StateLensEdit :: (Eq state) => FloatingLens state a b -> state -> Edit b -> Edit a;
		FunctorOneEdit :: (FunctorOne f) => Edit b -> Edit (f b);
	};
	
	applyAndInvertEdit :: a -> Edit a -> (a,Maybe (Edit a));
	applyAndInvertEdit olda (ReplaceEdit newa) = (newa,Just (ReplaceEdit olda));
	applyAndInvertEdit olda (StateLensEdit lens oldstate editb) = result where
	{
		oldb = lensGet lens oldstate olda;
		(newb,invb) = applyAndInvertEdit oldb editb;
		result = case lensPutback lens oldstate newb olda of
		{
			Just (newa,newstate) -> (newa,fmap (StateLensEdit lens newstate) invb);
			_ -> error "bad lens edit";
		};
	};
	applyAndInvertEdit oldfb (FunctorOneEdit editb) = (newfb,mneweditfb) where
	{
		newfb = fmap (applyEdit editb) oldfb;
		mneweditfb = case (retrieveOne oldfb) of
		{
			Left _ -> Nothing;
			Right oldb -> fmap FunctorOneEdit (invertEdit oldb editb);
		};
	};
	
	applyEdit :: Edit a -> a -> a;
	applyEdit edit olda = fst (applyAndInvertEdit olda edit);
	
	applyEdits :: [Edit a] -> a -> a;
	applyEdits [] = id;
	applyEdits (e:es) = (applyEdits es) . (applyEdit e);
	
	invertEdit :: a -> Edit a -> Maybe (Edit a);
	invertEdit olda edit = snd (applyAndInvertEdit olda edit);
	
	commutableEdits :: (Eq a) => Edit a -> Edit a -> a -> Maybe a;
	commutableEdits e1 e2 a = let
	{
		a1 = applyEdit e1 (applyEdit e2 a);
		a2 = applyEdit e2 (applyEdit e1 a);
	} in if a1 == a2 then Just a1 else Nothing;
	
	data FloatingLens state a b = MkFloatingLens
	{
		lensWitness :: LensWitness a b,
		lensStateWitness :: LensWitness a state,
		lensUpdate :: a -> Edit a -> state -> (state,Maybe (Edit b)),
		lensGet :: state -> a -> b,
		lensPutback :: state -> b -> a -> Maybe (a,state)
	};

	matchLens :: FloatingLens state1 a b1 -> FloatingLens state2 a b2 -> Maybe (EqualType state1 state2,EqualType b1 b2);
	matchLens lens1 lens2 = do
	{
		MkEqualType <- matchWitness (lensWitness lens1) (lensWitness lens2);
		MkEqualType <- matchWitness (lensStateWitness lens1) (lensStateWitness lens2);
		return (MkEqualType,MkEqualType);
	};

	matchTLens :: Type a -> FloatingLens state1 a b1 -> FloatingLens state2 a b2 -> Maybe (EqualType state1 state2,EqualType b1 b2);
	matchTLens _ = matchLens;

	functorOneLens :: forall f state a b. (FunctorOne f) => FloatingLens state a b -> FloatingLens state (f a) (f b);
	functorOneLens lens = MkFloatingLens
	{
		lensWitness = let
		{
			tfc = ConstTFComposite (unsafeIOWitnessFromString "Data.Changes.Edit.functorOne" :: IOWitness (TFApply TFMap));

			ff :: LensWitness x y -> LensWitness (f x) (f y);
			ff (MkTFWitness tc) = MkTFWitness (ApplyTFComposite tfc tc);
		} in ff (lensWitness lens),
		lensStateWitness = let
		{
			tfc = ConstTFComposite (unsafeIOWitnessFromString "Data.Changes.Edit.functorOneState" :: IOWitness (TFConverse TFB TFMatch));

			ff :: LensWitness x state -> LensWitness (f x) state;
			ff (MkTFWitness tc) = MkTFWitness (ApplyTFComposite tfc tc);
		} in ff (lensStateWitness lens),
		lensUpdate = update,
		lensGet = get,
		lensPutback = putback
	}
	where
	{
		update :: f a -> Edit (f a) -> state -> (state,Maybe (Edit (f b)));
		update fa (FunctorOneEdit ea) state | Right a <- retrieveOne fa = let
		{
			(state',meb) = lensUpdate lens a ea state;
		} in (state',fmap FunctorOneEdit meb);
		update fa efa state = (state,Just (ReplaceEdit (get state (applyEdit efa fa))));
		
		get state = fmap (lensGet lens state);

		putback state fb fa = case retrieveOne fb of
		{
			-- Left fa' -> Just (fa',state);
			Left _ -> Nothing;		-- pushing nothing on something fails
			Right b -> case retrieveOne fa of
			{
				Left _ -> Nothing;	-- pushing something on nothing fails
				Right a -> do
				{
					(a',state') <- lensPutback lens state b a;
					return (fmap (\_ -> a') fa,state');
				};
			};
		};
	};

	data FixedLens a b = MkFixedLens
	{
		fixedLensWitness :: LensWitness a b,
		fixedLensUpdate :: a -> Edit a -> Maybe (Edit b),
		fixedLensGet :: a -> b,
		fixedLensPutback :: b -> a -> Maybe a
	};
	
	voidStateWitness :: LensWitness a ();
	voidStateWitness = makeLensWitness (unsafeIOWitnessFromString "Data.Changes.Edit.void.state" :: IOWitness (TFConst ()));
	
	fixedFloatingLens :: FixedLens a b -> FloatingLens () a b;
	fixedFloatingLens lens = MkFloatingLens
	{
		lensWitness = fixedLensWitness lens,
		lensStateWitness = voidStateWitness,
		lensUpdate = \a edit _ -> ((),fixedLensUpdate lens a edit),
		lensGet = \_ -> fixedLensGet lens,
		lensPutback = \_ b a -> fmap (\newa -> (newa,())) (fixedLensPutback lens b a)
	};
	
	data SimpleLens a b = MkSimpleLens
	{
		simpleLensWitness :: LensWitness a b,
		simpleLensGet :: a -> b,
		simpleLensPutback :: b -> a -> Maybe a
	};
	
	simpleFixedLens :: SimpleLens a b -> FixedLens a b;
	simpleFixedLens lens = MkFixedLens
	{
		fixedLensWitness = simpleLensWitness lens,
		fixedLensUpdate = \a edit -> Just (ReplaceEdit (simpleLensGet lens (applyEdit edit a))),
		fixedLensGet = simpleLensGet lens,
		fixedLensPutback = simpleLensPutback lens
	};
	
	data WholeLens a b = MkWholeLens
	{
		wholeLensWitness :: LensWitness a b,
		wholeLensGet :: a -> b,
		wholeLensPutback :: b -> Maybe a
	};
	
	wholeSimpleLens :: WholeLens a b -> SimpleLens a b;
	wholeSimpleLens lens = MkSimpleLens
	{
		simpleLensWitness = wholeLensWitness lens,
		simpleLensGet = wholeLensGet lens,
		simpleLensPutback = \b _ -> wholeLensPutback lens b
	};

	traversableWholeLens :: forall f a b. (Traversable f) => WholeLens a b -> WholeLens (f a) (f b);
	traversableWholeLens lens = MkWholeLens
	{
		wholeLensWitness = let
		{
			tfc = ConstTFComposite (unsafeIOWitnessFromString "Data.Changes.Edit.functor" :: IOWitness (TFApply TFMap));

			ff :: LensWitness x y -> LensWitness (f x) (f y);
			ff (MkTFWitness tc) = MkTFWitness (ApplyTFComposite tfc tc);
		} in ff (wholeLensWitness lens),
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
