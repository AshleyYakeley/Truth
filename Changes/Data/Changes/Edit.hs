module Data.Changes.Edit where
{
	import Data.Result;
	import Data.TypeFunc;
	import Data.FunctorOne;
	import Data.ConstFunction;
	import Data.Chain;
	import Data.OpenWitness;
	import Data.Witness;
	import Control.Applicative;
	import Control.Category;
	import Prelude hiding (id,(.));

	type LensWitness = Chain (TFWitness (TFMappable IOWitness));
	makeLensWitness :: IOWitness tf -> LensWitness x (TF tf x);
	makeLensWitness wit = singleChain (MkTFWitness (SimpleTFMappable wit));

	data Edit a where
	{
		ReplaceEdit :: a -> Edit a;
		StateLensEdit :: (Eq state) => FloatingLens state a b -> state -> Edit b -> Edit a;
		FunctorOneEdit :: (FunctorOne f) => Edit b -> Edit (f b);
	};
	
	applyAndInvertEditCF :: Edit a -> (ConstFunction a a,a -> Maybe (Edit a));
	applyAndInvertEditCF (ReplaceEdit newa) = (pure newa,Just . ReplaceEdit);
	applyAndInvertEditCF (StateLensEdit lens oldstate editb) = result where
	{
		oldaoldb = lensGet lens oldstate;
		(foldbnewb,oldbminvb) = applyAndInvertEditCF editb;
		
		mapa = do
		{
			newb <- cofmap1CF oldaoldb foldbnewb;
			mas <- lensPutback lens oldstate newb;
			case mas of
			{
				Just as -> return as;
				_ -> error "bad lens edit";
			};
		};
		
		result = (fmap fst mapa,\olda -> let
		{
			minvb = oldbminvb (oldaoldb olda);
			(_,newstate) = applyConstFunction mapa olda;
			minva = fmap (StateLensEdit lens newstate) minvb;
		} in minva);
	};
	applyAndInvertEditCF (FunctorOneEdit editb) = (cfmap (applyEditCF editb),mneweditfb) where
	{
		mneweditfb = \oldfb -> case (retrieveOne oldfb) of
		{
			SuccessResult oldb -> fmap FunctorOneEdit (invertEditCF editb oldb);
			_ -> Nothing;
		};
	};
	
	applyEditCF :: Edit a -> ConstFunction a a;
	applyEditCF edit = fst (applyAndInvertEditCF edit);
	
	applyEditsCF :: [Edit a] -> ConstFunction a a;
	applyEditsCF [] = id;
	applyEditsCF (e:es) = (applyEditsCF es) . (applyEditCF e);
	
	invertEditCF :: Edit a -> a -> Maybe (Edit a);
	invertEditCF edit = snd (applyAndInvertEditCF edit);
	
	applyAndInvertEdit :: a -> Edit a -> (a,Maybe (Edit a));
	applyAndInvertEdit a edit = (applyConstFunction ae a,ie a) where
	{
		(ae,ie) = applyAndInvertEditCF edit;
	};
	
	applyEdit :: Edit a -> a -> a;
	applyEdit = applyConstFunction . applyEditCF;
	
	applyEdits :: [Edit a] -> a -> a;
	applyEdits = applyConstFunction . applyEditsCF;
	
	invertEdit :: a -> Edit a -> Maybe (Edit a);
	invertEdit olda edit =invertEditCF edit olda;
	
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
		lensUpdate :: Edit a -> state -> ConstFunction a (state,Maybe (Edit b)),
		lensGet :: state -> a -> b,
		lensPutback :: state -> b -> ConstFunction a (Maybe (a,state))
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
		lensWitness = cfmap (lensWitness lens),
		lensStateWitness = let
		{
			wit :: IOWitness TFMatch;
			wit = unsafeIOWitnessFromString "Data.Changes.Edit.functorOneState";
		} in (lensStateWitness lens) . (singleChain (MkTFWitness (SimpleTFMappable wit))),
		lensUpdate = updateCF,
		lensGet = get,
		lensPutback = putback
	}
	where
	{
		updateCF :: Edit (f a) -> state -> ConstFunction (f a) (state,Maybe (Edit (f b)));
		updateCF (FunctorOneEdit ea) state = do 
		{
			eit <- cofmap1CF retrieveOne (cfmap (lensUpdate lens ea state));
			case eit of
			{
				SuccessResult (state',meb) -> return (state',fmap FunctorOneEdit meb);
				_ -> return (state,Nothing);
			};
		};
		updateCF efa state = fmap (\newfa -> (state,Just (ReplaceEdit (get state newfa)))) (applyEditCF efa);

		get state = fmap (lensGet lens state);

		--lensPutback :: state -> (f b) -> ConstFunction (f a) (Maybe (f a,state))
		putback state fb = case retrieveOne fb of
		{
			SuccessResult b -> FunctionConstFunction (\fa -> case retrieveOne fa of
			{
				SuccessResult a -> do
				{
					(a',state') <- applyConstFunction (lensPutback lens state b) a;
					return (fmap (\_ -> a') fa,state');
				};
				_ -> Nothing;	-- pushing something on nothing fails
			});
			-- FailureResult fa' -> Just (fa',state);
			_ -> pure Nothing;		-- pushing nothing on something fails
		};
	};
}
