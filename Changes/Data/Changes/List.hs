module Data.Changes.List(listElement,listSection) where
{
	import Data.Changes.Edit;
	import Data.Witness;
	import Data.OpenWitness;
	import Control.Monad;
	import Data.Maybe;
	import Data.TypeFunc;
	import Prelude hiding (id,(.));

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
		

	listElement :: forall e. FloatingLens Int [e] (Maybe e);
	listElement = MkFloatingLens
	{
		lensWitness = elementWitness,
		lensStateWitness = elementStateWitness,
		lensUpdate = elementUpdate,
		lensGet = \i list -> if i < 0 then Nothing else elementGet i list,
		lensPutback = \i me list -> if i < 0 then Nothing else do
		{
			e <- me;
			newlist <- elementPutback i e list;
			return (newlist,i);
		}
	} where
	{
		elementWitness :: LensWitness [e] (Maybe e);
		elementWitness = makeLensWitness (unsafeIOWitnessFromString "Data.Changes.List.listElement" :: IOWitness 
			(TFCompose (TFApply Maybe) TFMatch)
			);

		elementStateWitness :: LensWitness [e] Int;
		elementStateWitness = makeLensWitness (unsafeIOWitnessFromString "Data.Changes.List.listElement.state" :: IOWitness
			(TFConst Int)
			);
	
		elementGet :: Int -> [e] -> Maybe e;
		elementGet 0 (e:_) = Just e;
		elementGet _ [] = Nothing;
		elementGet i (_:es) = elementGet (i - 1) es;
		
		elementPutback :: Int -> e -> [e] -> Maybe [e];
		elementPutback _ _ [] = Nothing;
		elementPutback 0 x (_:es) = Just (x:es);
		elementPutback i x (e:es) = do
		{
			xs <- elementPutback (i - 1) x es;
			return (e:xs);
		};
	
		elementUpdate :: [e] -> Edit [e] -> Int -> (Int,Maybe (Edit (Maybe e)));
		elementUpdate olda edita state = 
		  fromMaybe (state,Just (ReplaceEdit (lensGet listElement state (applyEdit edita olda)))) update_ where
		{
			update_ :: Maybe (Int,Maybe (Edit (Maybe e)));
			update_ = case edita of
			{
				StateLensEdit editlens editlensstate editbedit -> do
				{
					(MkEqualType,MkEqualType) <- matchTLens (Type :: Type [e]) editlens listSection;
					let
					{
						oldb = lensGet editlens editlensstate olda;
						newb = applyEdit editbedit oldb;
						newlen = length newb;
						((newstate,_),mclip) = updateSection (state,1) editlensstate newlen;
					};
					--if (newstatelen > 0)
					-- then 
					return (newstate,fmap (\_ -> ReplaceEdit (elementGet newstate newb)) mclip);
				} `mplus` do
				{
					(MkEqualType,MkEqualType) <- matchTLens (Type :: Type [e]) editlens listElement;
					if (editlensstate == state)
					 then return (state,Just editbedit)
					 else return (state,Nothing);
				};
				_ -> Nothing;
			};
		};
	};

	listSection :: forall e. FloatingLens (Int,Int) [e] [e];
	listSection = MkFloatingLens
	{
		lensWitness = sectionWitness,
		lensStateWitness = sectionStateWitness,
		lensUpdate = sectionUpdate,
		lensGet = \(start,len) list -> take len (drop start list),
		lensPutback = \(start,len) newmiddle list -> let
		{
			(before,rest) = splitAt start list;
			(_,after) = splitAt len rest;
		} in Just (before ++ newmiddle ++ after,(start,length newmiddle))
	} where
	{
		sectionWitness :: LensWitness [e] [e];
		sectionWitness = makeLensWitness (unsafeIOWitnessFromString "Data.Changes.List.listSection" :: IOWitness TFIdentity);

		sectionStateWitness :: LensWitness [e] (Int,Int);
		sectionStateWitness = makeLensWitness (unsafeIOWitnessFromString "Data.Changes.List.listSection.state" :: IOWitness (TFConst (Int,Int)));
	
		sectionUpdate :: [e] -> Edit [e] -> (Int,Int) -> ((Int,Int),Maybe (Edit [e]));
		sectionUpdate olda edita state = 
		  fromMaybe (state,Just (ReplaceEdit (lensGet listSection state (applyEdit edita olda)))) update_ where
		{
			update_ :: Maybe ((Int,Int),Maybe (Edit [e]));
			update_ = case edita of
			{
				StateLensEdit editlens editlensstate editbedit -> do
				{
					(MkEqualType,MkEqualType) <- matchTLens (Type :: Type [e]) editlens listSection;
					let
					{
						oldb = lensGet editlens editlensstate olda;
						newb = applyEdit editbedit oldb;
						newlen = length newb;
						(newstate,mclip) = updateSection state editlensstate newlen;
					};
					return (newstate,fmap (\clip -> StateLensEdit editlens clip editbedit) mclip);
				} `mplus` do
				{
					(MkEqualType,MkEqualType) <- matchTLens (Type :: Type [e]) editlens listElement;
					let
					{
						oldb = lensGet editlens editlensstate olda;
						newb = applyEdit editbedit oldb;
						newlen = case newb of
						{
							Just _ -> 1;
							_ -> 0;
						};
						(newstate,mclip) = updateSection state (editlensstate,1) newlen;
					};
					return (newstate,fmap (\(clip,_) -> StateLensEdit editlens clip editbedit) mclip);					
				};
				_ -> Nothing;
			};
		};
	};
}
