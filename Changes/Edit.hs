module Edit where
{
	import ValueType;
	import Data.Witness;
	import Data.OpenWitness;
	import Data.Maybe;
	import TypeFunc;
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

	compareEdit :: (Eq a) => ValueType a -> a -> a -> [Edit a];
	compareEdit _ old new | old == new = [];
	compareEdit _ _ new = [ReplaceEdit new];
	
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

	section :: forall e. StateLens (Int,Int) [e] [e];
	section = MkStateLens
	{
		slensWitness = sectionWitness,
		slensStateWitness = sectionStateWitness,
		slensUpdate = sectionUpdate,
		slensGet = \(start,len) list -> take len (drop start list),
		slensPutback = \(start,len) newmiddle list -> let
		{
			(before,rest) = splitAt start list;
			(_,after) = splitAt len rest;
		} in Just (before ++ newmiddle ++ after,(start,length newmiddle))
	} where
	{
		sectionWitness :: TFWitness IOWitness [e] [e];
		sectionWitness = MkTFWitness (unsafeIOWitnessFromString "Ghide.Edit.section" :: IOWitness TFIdentity);

		sectionStateWitness :: TFWitness IOWitness [e] (Int,Int);
		sectionStateWitness = MkTFWitness (unsafeIOWitnessFromString "Ghide.Edit.section.state" :: IOWitness (TFConst (Int,Int)));
	
		sectionUpdate :: [e] -> Edit [e] -> (Int,Int) -> ((Int,Int),Maybe (Edit [e]));
		sectionUpdate olda edita state = 
		  fromMaybe (state,Just (ReplaceEdit (slensGet section state (applyEdit edita olda)))) update_ where
		{
		{-
			toNatural :: Int -> Either Int Int;
			toNatural n | n >= 0 = Right n;
			toNatural n = Left (-n);
			
			naturalDiff :: Int -> Int -> Either Int Int;
			naturalDiff a b = toNatural (a - b);
		-}
			updateSection :: (Int,Int) -> (Int,Int) -> Int -> ((Int,Int),Maybe (Int,Int));
			updateSection (start,len) (editstart,editlen) newlen = let
			{
				end = start + len;
				editend = editstart + editlen;
			} in if editend == start && editstart == end then ((start,newlen),Just (0,0)) -- point expansion
			 else if editend <= start then ((start+newlen-editlen,len),Nothing) -- entirely before
			 else if editstart >= end then ((start,len),Nothing) -- entirely after
			 else if editstart < start
			   then if editend > end	-- clip start
			     then ((editstart,newlen),Just (0,len)) -- clip both
			     else ((editstart,end-editend+newlen),Just (0,editend-start)) -- clip start, no clip end
			   else if editend > end    -- no clip start
			     then ((start,editstart-start+newlen),Just (editstart-start,len-editstart+start)) -- no clip start, clip end
			     else ((start,len+newlen-editlen),Just (editstart-start,editlen)); -- within
		
			update_ :: Maybe ((Int,Int),Maybe (Edit [e]));
			update_ = case edita of
			{
				StateLensEdit editlens editlensstate editbedit -> do
				{
					MkEqualType <- matchTWitness (Type :: Type [e]) sectionWitness (slensWitness editlens);
					MkEqualType <- matchWitness sectionStateWitness (slensStateWitness editlens);
					let
					{
						oldb = slensGet editlens editlensstate olda;
						newb = applyEdit editbedit oldb;
						newlen = length newb;
						(newstate,mclip) = updateSection state editlensstate newlen;
					};
					return (newstate,fmap (\clip -> StateLensEdit editlens clip editbedit) mclip);
				};
				_ -> Nothing;
			};
		};
	};
}
