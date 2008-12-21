module Data.Changes.List where
{
	import Data.Changes.Edit;
	import Data.Witness;
	import Data.OpenWitness;
	import Data.Maybe;
	import Data.TypeFunc;
	import Prelude hiding (id,(.));

	listSection :: forall e. StateLens (Int,Int) [e] [e];
	listSection = MkStateLens
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
		  fromMaybe (state,Just (ReplaceEdit (slensGet listSection state (applyEdit edita olda)))) update_ where
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
