module Data.Changes.Edit where
{
	import Data.FunctorOne;
	import Data.ConstFunction;
	import Data.Result;
	import Data.Chain;
	import Control.Applicative;
	import Control.Category;
	import Data.Word;
	import Prelude hiding (id,(.));

	class EditScheme partedit a where
	{
		applyPartEdit :: partedit -> ConstFunction a a;
		invertPartEdit :: partedit -> a -> Maybe (Edit a);	-- "Nothing" means no change
	};

	class (EditScheme (PartEdit a) a) => Editable a where
	{
		type PartEdit a :: *;
	};

	instance Editable () where
	{
		type PartEdit () = Nothing;
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

	data Nothing;
	
	never :: Nothing -> a;
	never n = seq n undefined;

	instance EditScheme Nothing a where
	{
		applyPartEdit _ = id;
		invertPartEdit _ _ = Nothing;
	};

	newtype JustEdit a = JustEdit (Edit a);

	instance (Editable a,FunctorOne f,PartEdit (f a) ~ JustEdit a) => EditScheme (JustEdit a) (f a) where
	{
		applyPartEdit (JustEdit edita) = cfmap (applyEditCF edita);

		invertPartEdit (JustEdit edita) molda = case retrieveOne molda of
		{
			SuccessResult olda -> fmap (PartEdit . JustEdit) (invertEditCF edita olda);
			_ -> Nothing;
		};
	};

	instance (Editable a) => Editable (Result err a) where
	{
		type PartEdit (Result err a) = JustEdit a;
	};

	instance (Editable a) => Editable (Maybe a) where
	{
		type PartEdit (Maybe a) = JustEdit a;
	};

	applyEditCF :: (Editable a) => Edit a -> ConstFunction a a;
	applyEditCF (ReplaceEdit newa) = pure newa;
	applyEditCF (PartEdit pea) = applyPartEdit pea;

	invertEditCF :: (Editable a) => Edit a -> a -> Maybe (Edit a);
	invertEditCF (ReplaceEdit _) olda = Just (ReplaceEdit olda);
	invertEditCF (PartEdit pea) olda = invertPartEdit pea olda;

	applyAndInvertEditCF :: (Editable a) => Edit a -> (ConstFunction a a,a -> Maybe (Edit a));
	applyAndInvertEditCF edit = (applyEditCF edit,invertEditCF edit);

	extractJustEdit :: Edit (Maybe a) -> Maybe (Edit a);
	extractJustEdit (PartEdit (JustEdit ea)) = Just ea; 
	extractJustEdit (ReplaceEdit (Just a)) = Just (ReplaceEdit a); 
	extractJustEdit (ReplaceEdit Nothing) = Nothing; 
	
	applyEditsCF :: (Editable a) => [Edit a] -> ConstFunction a a;
	applyEditsCF [] = id;
	applyEditsCF (e:es) = (applyEditsCF es) . (applyEditCF e);

	commutableEdits :: (Editable a,Eq a) => Edit a -> Edit a -> a -> Maybe a;
	commutableEdits e1 e2 a = let
	{
		cf1 = applyEditCF e1;
		cf2 = applyEditCF e2;
		cf12 = cf1 . cf2;
		cf21 = cf2 . cf1;
		a12 = applyConstFunction cf12 a;
		a21 = applyConstFunction cf21 a;
	} in if a12 == a21 then Just a12 else Nothing;

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
