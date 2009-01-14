module Data.Changes.Edit where
{
	import Data.Codec;
	import Data.TypeFunc;
	import Data.OpenWitness;
	import Control.Category;
	import Prelude hiding (id,(.));

	type LensWitness = TFWitness (TFComposite IOWitness);
	makeLensWitness :: IOWitness tf -> LensWitness x (TF tf x);
	makeLensWitness wit = MkTFWitness (ConstTFComposite wit);

	class (Functor f) => FunctorOne f where
	{
		retrieveOne :: f a -> Either (f b) a;
	};
	-- retrieveOne (fmap f w) = fmap f (retrieveOne w)
	-- case (retrieveOne w) of {Left w' -> fmap f w';Right a -> fmap (\_ -> a) w;} = w

	instance FunctorOne Maybe where
	{
		retrieveOne (Just a) = Right a;
		retrieveOne Nothing = Left Nothing;
	};
	
	instance FunctorOne (Either a) where
	{
		retrieveOne (Right b) = Right b;
		retrieveOne (Left a) = Left (Left a);
	};

	data Edit a where
	{
		ReplaceEdit :: a -> Edit a;
		StateLensEdit :: (Eq state) => StateLens state a b -> state -> Edit b -> Edit a;
		FunctorOneEdit :: (FunctorOne f) => Edit b -> Edit (f b);
	};
	
	applyAndInvertEdit :: a -> Edit a -> (a,Maybe (Edit a));
	applyAndInvertEdit olda (ReplaceEdit newa) = (newa,Just (ReplaceEdit olda));
	applyAndInvertEdit olda (StateLensEdit lens oldstate editb) = result where
	{
		oldb = slensGet lens oldstate olda;
		(newb,invb) = applyAndInvertEdit oldb editb;
		result = case slensPutback lens oldstate newb olda of
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
	
	data StateLens state a b = MkStateLens
	{
		slensWitness :: LensWitness a b,
		slensStateWitness :: LensWitness a state,
		slensUpdate :: a -> Edit a -> state -> (state,Maybe (Edit b)),
		slensGet :: state -> a -> b,
		slensPutback :: state -> b -> a -> Maybe (a,state)
	};

	functorOneLens :: forall f state a b. (FunctorOne f) => StateLens state a b -> StateLens state (f a) (f b);
	functorOneLens lens = MkStateLens
	{
		slensWitness = let
		{
			tfc = ConstTFComposite (unsafeIOWitnessFromString "Data.Changes.Edit.functorOne" :: IOWitness (TFApply TFMap));

			ff :: LensWitness x y -> LensWitness (f x) (f y);
			ff (MkTFWitness tc) = MkTFWitness (ApplyTFComposite tfc tc);
		} in ff (slensWitness lens),
		slensStateWitness = let
		{
			tfc = ConstTFComposite (unsafeIOWitnessFromString "Data.Changes.Edit.functorOneState" :: IOWitness (TFConverse TFB TFMatch));

			ff :: LensWitness x state -> LensWitness (f x) state;
			ff (MkTFWitness tc) = MkTFWitness (ApplyTFComposite tfc tc);
		} in ff (slensStateWitness lens),
		slensUpdate = update,
		slensGet = get,
		slensPutback = putback
	}
	where
	{
		update :: f a -> Edit (f a) -> state -> (state,Maybe (Edit (f b)));
		update fa (FunctorOneEdit ea) state | Right a <- retrieveOne fa = let
		{
			(state',meb) = slensUpdate lens a ea state;
		} in (state',fmap FunctorOneEdit meb);
		update fa efa state = (state,Just (ReplaceEdit (get state (applyEdit efa fa))));
		
		get state = fmap (slensGet lens state);

		putback state fb fa = case retrieveOne fb of
		{
			-- Left fa' -> Just (fa',state);
			Left _ -> Nothing;		-- pushing nothing on something fails
			Right b -> case retrieveOne fa of
			{
				Left _ -> Nothing;	-- pushing something on nothing fails
				Right a -> do
				{
					(a',state') <- slensPutback lens state b a;
					return (fmap (\_ -> a') fa,state');
				};
			};
		};
	};

	data SimpleLens a b = MkSimpleLens
	{
		simpleLensWitness :: LensWitness a b,
		simpleLensUpdate :: a -> Edit a -> Maybe (Edit b),
		simpleLensGet :: a -> b,
		simpleLensPutback :: b -> a -> Maybe a
	};
	
	voidStateWitness :: LensWitness a ();
	voidStateWitness = makeLensWitness (unsafeIOWitnessFromString "Data.Changes.Edit.void.state" :: IOWitness (TFConst ()));
	
	simpleStateLens :: SimpleLens a b -> StateLens () a b;
	simpleStateLens lens = MkStateLens
	{
		slensWitness = simpleLensWitness lens,
		slensStateWitness = voidStateWitness,
		slensUpdate = \a edit _ -> ((),simpleLensUpdate lens a edit),
		slensGet = \_ -> simpleLensGet lens,
		slensPutback = \_ b a -> fmap (\newa -> (newa,())) (simpleLensPutback lens b a)
	};
	
	codecSimpleLens :: LensWitness a (Maybe b) -> Codec a b -> SimpleLens a (Maybe b);
	codecSimpleLens wit codec = MkSimpleLens
	{
		simpleLensWitness = wit,
		simpleLensUpdate = \a edit -> Just (ReplaceEdit (decode codec (applyEdit edit a))),
		simpleLensGet = decode codec,
		simpleLensPutback = \mb _ -> fmap (encode codec) mb
	};
}
