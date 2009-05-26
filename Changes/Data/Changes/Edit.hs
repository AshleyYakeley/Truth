module Data.Changes.Edit where
{
--	import Data.Result;
	import Data.TypeFunc;
	import Data.FunctorOne;
	import Data.ConstFunction;
	import Data.Chain;
	import Data.OpenWitness;
	import Data.Witness;
	import Control.Applicative;
	import Control.Category;
	import Data.Word;
	import Prelude hiding (id,(.));

	type LensWitness = Chain (TFWitness (TFMappable IOWitness));
	makeLensWitness :: IOWitness tf -> LensWitness x (TF tf x);
	makeLensWitness wit = singleChain (MkTFWitness (SimpleTFMappable wit));

	class EditScheme partedit a where
	{
		applyPartEdit :: partedit -> ConstFunction a a;
		invertPartEdit :: partedit -> a -> Maybe (Edit a);	-- "Nothing" means no change
	};

	class (EditScheme (PartEdit a) a) => Editable a where
	{
		type PartEdit a :: *;
--		applyPartEdit :: PartEdit a -> ConstFunction a a;
--		invertPartEdit :: PartEdit a -> a -> Maybe (Edit a);	-- "Nothing" means no change
	};

	instance Editable () where
	{
		type PartEdit () = Nothing;
--		data PartEdit ();
--		applyPartEdit _ = id;
--		invertPartEdit _ _ = Nothing;
	};

	instance Editable Bool where
	{
		type PartEdit Bool = Nothing;
	};

	instance Editable Word8 where
	{
		type PartEdit Word8 = Nothing;
	};

	data Edit a = ReplaceEdit a | PartEdit (PartEdit a);

--	type Edit a = Edit' (PartEdit a) a;

	data Nothing;

	instance EditScheme Nothing a where
	{
		applyPartEdit _ = id;
		invertPartEdit _ _ = Nothing;
	};

	newtype JustEdit a = JustEdit (Edit a);

	instance (Editable a) => EditScheme (JustEdit a) (Maybe a) where
	{
		applyPartEdit (JustEdit edita) = cfmap (applyEditCF edita);

		invertPartEdit (JustEdit edita) (Just olda) = fmap (PartEdit . JustEdit) (invertEditCF edita olda);
		invertPartEdit (JustEdit _) _ = Nothing;
	};

	applyEditCF :: (Editable a) => Edit a -> ConstFunction a a;
	applyEditCF (ReplaceEdit newa) = pure newa;
	applyEditCF (PartEdit pea) = applyPartEdit pea;

	invertEditCF :: (Editable a) => Edit a -> a -> Maybe (Edit a);
	invertEditCF (ReplaceEdit _) olda = Just (ReplaceEdit olda);
	invertEditCF (PartEdit pea) olda = invertPartEdit pea olda;

	applyAndInvertEditCF :: (Editable a) => Edit a -> (ConstFunction a a,a -> Maybe (Edit a));
	applyAndInvertEditCF edit = (applyEditCF edit,invertEditCF edit);
{-
	applyEditCF :: (Editable a) => Edit a -> ConstFunction a a;
	applyEditCF (ReplaceEdit newa) = pure newa;
	applyEditCF (PartEdit pea) = applyPartEdit pea;

	invertEditCF :: (Editable a) => Edit a -> a -> Maybe (Edit a);
	invertEditCF (ReplaceEdit _) olda = Just (ReplaceEdit olda);
	invertEditCF (PartEdit pea) olda = invertPartEdit pea olda;
-}
	instance (Editable a) => Editable (Maybe a) where
	{
		type PartEdit (Maybe a) = JustEdit a;
--		data PartEdit (Maybe a) = JustEdit (Edit a);

--		applyPartEdit (JustEdit edita) = cfmap (applyEditCF edita);

--		invertPartEdit (JustEdit edita) (Just olda) = fmap (PartEdit . JustEdit) (invertEditCF edita olda);
--		invertPartEdit (JustEdit _) _ = Nothing;
	};

	extractJustEdit :: Edit (Maybe a) -> Maybe (Edit a);
	extractJustEdit (PartEdit (JustEdit ea)) = Just ea; 
	extractJustEdit (ReplaceEdit (Just a)) = Just (ReplaceEdit a); 
	extractJustEdit (ReplaceEdit Nothing) = Nothing; 
{-
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
-}	
--	applyEditCF :: Edit a -> ConstFunction a a;
--	applyEditCF edit = fst (applyAndInvertEditCF edit);
	
	applyEditsCF :: (Editable a) => [Edit a] -> ConstFunction a a;
	applyEditsCF [] = id;
	applyEditsCF (e:es) = (applyEditsCF es) . (applyEditCF e);
	
--	invertEditCF :: Edit a -> a -> Maybe (Edit a);
--	invertEditCF edit = snd (applyAndInvertEditCF edit);
	
	applyAndInvertEdit :: (Editable a) => a -> Edit a -> (a,Maybe (Edit a));
	applyAndInvertEdit a edit = (applyConstFunction ae a,ie a) where
	{
		(ae,ie) = applyAndInvertEditCF edit;
	};
	
	applyEdit :: (Editable a) => Edit a -> a -> a;
	applyEdit = applyConstFunction . applyEditCF;
	
	applyEdits :: (Editable a) => [Edit a] -> a -> a;
	applyEdits = applyConstFunction . applyEditsCF;
	
	invertEdit :: (Editable a) => a -> Edit a -> Maybe (Edit a);
	invertEdit olda edit =invertEditCF edit olda;
	
	commutableEdits :: (Editable a,Eq a) => Edit a -> Edit a -> a -> Maybe a;
	commutableEdits e1 e2 a = let
	{
		a1 = applyEdit e1 (applyEdit e2 a);
		a2 = applyEdit e2 (applyEdit e1 a);
	} in if a1 == a2 then Just a1 else Nothing;
{-	
	class PutType q where
	{
		toPutMaybe :: q a b -> ConstFunction a (Maybe b);
	}
	
	instance 
-}	
	data FloatingLens' m state a b = MkFloatingLens
	{
		lensWitness :: LensWitness a b,
		lensStateWitness :: LensWitness a state,
		lensUpdate :: Edit a -> state -> ConstFunction a (state,Maybe (Edit b)),
		lensGet :: state -> a -> b,
--		lensPutback :: state -> b -> ConstFunction a (Maybe (a,state)),
		lensPutEdit :: state -> Edit b -> ConstFunction a (m (Edit a))	-- m failure means impossible
	};
	
	type FloatingLens = FloatingLens' Maybe;
	
	toFloatingLens :: (FunctorOne m) => FloatingLens' m state a b -> FloatingLens state a b;
	toFloatingLens lens = MkFloatingLens
	{
		lensWitness = lensWitness lens,
		lensStateWitness = lensStateWitness lens,
		lensUpdate = lensUpdate lens,
		lensGet = lensGet lens,
		lensPutEdit = \state edit -> fmap getMaybeOne (lensPutEdit lens state edit)
	};

	
{-
	lensPutback' :: (Editable a) => FloatingLens state a b -> state -> b -> ConstFunction a (Maybe (a,state));
	lensPutback' lens state b = case lensPutEdit lens state (ReplaceEdit b) of
	{
		Just (edita,newstate) -> do
		{
			newa <- applyEditCF edita;
			return (Just (newa,newstate));
		};
		Nothing -> pure Nothing;
	};
-}
	matchLens :: FloatingLens' m state1 a b1 -> FloatingLens' m state2 a b2 -> Maybe (EqualType state1 state2,EqualType b1 b2);
	matchLens lens1 lens2 = do
	{
		MkEqualType <- matchWitness (lensWitness lens1) (lensWitness lens2);
		MkEqualType <- matchWitness (lensStateWitness lens1) (lensStateWitness lens2);
		return (MkEqualType,MkEqualType);
	};

	matchTLens :: Type a -> FloatingLens' m state1 a b1 -> FloatingLens' m state2 a b2 -> Maybe (EqualType state1 state2,EqualType b1 b2);
	matchTLens _ = matchLens;

{-
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
		updateCF efa state = fmap (\newfa -> (state,Just (replaceEdit (get state newfa)))) (applyEditCF efa);

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
-}
}
