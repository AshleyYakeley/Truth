module Data.Changes.EditScheme where
{
	import Data.ConstFunction;
	import Control.Category;
	import Prelude hiding (id,(.));

	class EditScheme a edit where
	{
        applyEdit :: edit -> ConstFunction a a;
    	invertEdit :: edit -> a -> Maybe edit;	-- "Nothing" means no change
		{-
		applyPartEdit :: partedit -> ConstFunction a a;
		invertPartEdit :: partedit -> a -> Maybe (Edit' partedit a);	-- "Nothing" means no change
		-}
	};

{-
	applyEdit (ReplaceEdit newa) = pure newa;
	applyEdit (PartEdit pea) = applyPartEdit pea;

	invertEdit (ReplaceEdit _) olda = Just (ReplaceEdit olda);
	invertEdit (PartEdit pea) olda = invertPartEdit pea olda;
-}
	applyAndInvertEdit :: (EditScheme a edit) => edit -> (ConstFunction a a,a -> Maybe edit);
	applyAndInvertEdit edit = (applyEdit edit,invertEdit edit);
	
	applyEdits :: (EditScheme a edit) => [edit] -> ConstFunction a a;
	applyEdits [] = id;
	applyEdits (e:es) = (applyEdits es) . (applyEdit e);

	commutableEdits :: (EditScheme a edit,Eq a) => edit -> edit -> a -> Maybe a;
	commutableEdits e1 e2 a = let
	{
		cf1 = applyEdit e1;
		cf2 = applyEdit e2;
		cf12 = cf1 . cf2;
		cf21 = cf2 . cf1;
		a12 = applyConstFunction cf12 a;
		a21 = applyConstFunction cf21 a;
	} in if a12 == a21 then Just a12 else Nothing;
	
	class (EditScheme a edit) => CompleteEditScheme a edit where
	{
	    replaceEdit :: a -> edit;
	};
}
