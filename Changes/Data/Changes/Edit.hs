module Data.Changes.Edit where
{
	import Data.OpenWitness;
	import Data.TypeFunc;
	import Control.Category;
	import Prelude hiding (id,(.));

	data Edit a where
	{
		ReplaceEdit :: a -> Edit a;
		StateLensEdit :: (Eq state) => StateLens state a b -> state -> Edit b -> Edit a;
	};
	
	applyAndInvertEdit :: a -> Edit a -> (a,Edit a);
	applyAndInvertEdit olda (ReplaceEdit newa) = (newa,ReplaceEdit olda);
	applyAndInvertEdit olda (StateLensEdit lens oldstate editb) = result where
	{
		oldb = slensGet lens oldstate olda;
		(newb,invb) = applyAndInvertEdit oldb editb;
		result = case slensPutback lens oldstate newb olda of
		{
			Just (newa,newstate) -> (newa,StateLensEdit lens newstate invb);
			_ -> error "bad lens edit";
		};
	};
	
	applyEdit :: Edit a -> a -> a;
	applyEdit edit olda = fst (applyAndInvertEdit olda edit);
	
	applyEdits :: [Edit a] -> a -> a;
	applyEdits [] = id;
	applyEdits (e:es) = (applyEdits es) . (applyEdit e);
	
	invertEdit :: a -> Edit a -> Edit a;
	invertEdit olda edit = snd (applyAndInvertEdit olda edit);
	
	commutableEdits :: (Eq a) => Edit a -> Edit a -> a -> Maybe a;
	commutableEdits e1 e2 a = let
	{
		a1 = applyEdit e1 (applyEdit e2 a);
		a2 = applyEdit e2 (applyEdit e1 a);
	} in if a1 == a2 then Just a1 else Nothing;
	
	data StateLens state a b = MkStateLens
	{
		slensWitness :: TFWitness IOWitness a b,
		slensStateWitness :: TFWitness IOWitness a state,
		slensUpdate :: a -> Edit a -> state -> (state,Maybe (Edit b)),
		slensGet :: state -> a -> b,
		slensPutback :: state -> b -> a -> Maybe (a,state)
	};
}
