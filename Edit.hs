module Edit where
{
	import ValueType;
	import Lens;
	import Data.Witness;
	import Data.List;

	data Edit a where
	{
		ReplaceEdit :: a -> Edit a;
		HunkEdit :: Int -> Int -> Edit [e] -> Edit [e];
		MapEdit :: (Functor f) => Edit a -> Edit (f a);
		LensEdit :: LensC a b -> Edit b -> Edit a;
		GeneralEdit :: (a -> a) -> Edit a;
	};
	
	applyEdit :: Edit a -> a -> a;
	applyEdit (ReplaceEdit a) _ = a;
	applyEdit (GeneralEdit mp) a = mp a;
	applyEdit (LensEdit lens edit) a = lensPutback lens (applyEdit edit (lensGet lens a)) a;
	applyEdit (MapEdit edit) a = fmap (applyEdit edit) a;
	applyEdit (HunkEdit start len edit) list = before ++ (applyEdit edit middle) ++ after where
	{
		(before,rest) = splitAt start list;
		(middle,after) = splitAt len rest;
	};
	
	applyEdits :: [Edit a] -> a -> a;
	applyEdits [] = id;
	applyEdits (e:es) = (applyEdits es) . (applyEdit e);
	
	invertEdit :: a -> Edit a -> Edit a;
	invertEdit a (LensEdit lens edit) = LensEdit lens (invertEdit (lensGet lens a) edit);
	invertEdit list (HunkEdit start len edit) = HunkEdit start len' (invertEdit middle' edit) where
	{
		(_,rest) = splitAt start list;
		(middle,_) = splitAt len rest;
		middle' = applyEdit edit middle;
		len' = length middle';
	};
	invertEdit a _ = ReplaceEdit a;

{-
	applyEdit (LensEdit lens edit) a = lensPutback lens (applyEdit edit (lensGet lens a)) a;
	invertEdit a (LensEdit lens edit) = LensEdit lens (invertEdit (lensGet lens a) edit);

	applyEdit (invertEdit a edit) (applyEdit edit a) = a
	applyEdit (invertEdit a (LensEdit lens edit)) (applyEdit (LensEdit lens edit) a) = a
	applyEdit (LensEdit lens (invertEdit (lensGet lens a) edit)) (lensPutback lens (applyEdit edit (lensGet lens a)) a) = a
	lensPutback lens (applyEdit (invertEdit (lensGet lens a) edit) (lensGet lens (lensPutback lens (applyEdit edit (lensGet lens a)) a))) (lensPutback lens (applyEdit edit (lensGet lens a)) a) = a
	lensPutback lens (applyEdit (invertEdit (lensGet lens a) edit) (applyEdit edit (lensGet lens a)) ) (lensPutback lens (applyEdit edit (lensGet lens a)) a) = a
	lensPutback lens (lensGet lens a) (lensPutback lens (applyEdit edit (lensGet lens a)) a) = a
	lensPutback lens (lensGet lens a) a = a
	a = a
-}

	compareEdit :: (Eq a) => ValueType a -> a -> a -> [Edit a];
	compareEdit _ old new | old == new = [];
	compareEdit _ _ new = [ReplaceEdit new];
	
	commutableEdits :: (Eq a) => Edit a -> Edit a -> a -> Maybe a;
	commutableEdits e1 e2 a = let
	{
		a1 = applyEdit e1 (applyEdit e2 a);
		a2 = applyEdit e2 (applyEdit e1 a);
	} in if a1 == a2 then Just a1 else Nothing;
	
	type AnyV = AnyF ValueType;
	
	data AnyLensC a = forall b. MkAnyLensC (ValueType b) (LensC a b);
	
	lensableType :: ValueType a -> Maybe [AnyLensC a];
	lensableType (CollectionValueType list) = Just (lensableListType list) where
	{
		consItem :: AnyLensC r -> AnyLensC (a,r);
		consItem (MkAnyLensC vt lens) = (MkAnyLensC vt (consLens lens));
	
		lensableListType :: ListType ValueType a -> [AnyLensC a];
		lensableListType NilListType = [];
		lensableListType (ConsListType vt rt) = (MkAnyLensC vt firstLens):(fmap consItem (lensableListType rt));
	};
	lensableType _ = Nothing;
}
