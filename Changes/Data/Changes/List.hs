module Data.Changes.List(listElement,listSection) where
{
	import Data.Changes.Edit;
	import Control.Arrow;
	import Data.ConstFunction;
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
		lensPutback = \i me -> if i < 0 then return Nothing else do
		{
			case me of
			{
				Nothing -> return Nothing;
				Just e -> arr (\list -> do
				{
					newlist <- elementPutback i e list;
					return (newlist,i);
				});
			};
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
	
		--lensUpdate :: Edit a -> state -> ConstFunction a (state,Maybe (Edit b)),

		elementUpdate :: Edit [e] -> Int -> ConstFunction [e] (Int,Maybe (Edit (Maybe e)));
		elementUpdate edita state = 
		  fromMaybe (fmap (\newa -> (state,Just (ReplaceEdit (lensGet listElement state newa)))) (applyEditCF edita)) update_ where
		{
			update_ :: Maybe (ConstFunction [e] (Int,Maybe (Edit (Maybe e))));
			update_ = case edita of
			{
				StateLensEdit editlens editlensstate editbedit -> do
				{
					(MkEqualType,MkEqualType) <- matchTLens (Type :: Type [e]) editlens listSection;
					return (FunctionConstFunction (\olda ->
					let
					{
						oldb = lensGet editlens editlensstate olda;
						newb = applyEdit editbedit oldb;
						newlen = length newb;
						((newstate,_),mclip) = updateSection (state,1) editlensstate newlen;
					} in (newstate,fmap (\_ -> ReplaceEdit (elementGet newstate newb)) mclip) ));
				} `mplus` do
				{
					(MkEqualType,MkEqualType) <- matchTLens (Type :: Type [e]) editlens listElement;
					return (return (state,
						if (editlensstate == state) then Just editbedit else Nothing
						));
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
		lensPutback = \(start,len) newmiddle -> arr (\list -> return (let
		{
			(before,rest) = splitAt start list;
			(_,after) = splitAt len rest;
		} in (before ++ newmiddle ++ after,(start,length newmiddle)) ))
	} where
	{
		sectionWitness :: LensWitness [e] [e];
		sectionWitness = makeLensWitness (unsafeIOWitnessFromString "Data.Changes.List.listSection" :: IOWitness TFIdentity);

		sectionStateWitness :: LensWitness [e] (Int,Int);
		sectionStateWitness = makeLensWitness (unsafeIOWitnessFromString "Data.Changes.List.listSection.state" :: IOWitness (TFConst (Int,Int)));
	
		sectionUpdate :: Edit [e] -> (Int,Int) -> ConstFunction [e] ((Int,Int),Maybe (Edit [e]));
		sectionUpdate edita state = 
		  fromMaybe (fmap (\newa -> (state,Just (ReplaceEdit (lensGet listSection state newa)))) (applyEditCF edita)) update_ where
		{
			update_ :: Maybe (ConstFunction [e] ((Int,Int),Maybe (Edit [e])));
			update_ = case edita of
			{
				StateLensEdit editlens editlensstate editbedit -> do
				{
					(MkEqualType,MkEqualType) <- matchTLens (Type :: Type [e]) editlens listSection;
					return (FunctionConstFunction (\olda ->
					let
					{
						oldb = lensGet editlens editlensstate olda;
						newb = applyEdit editbedit oldb;
						newlen = length newb;
						(newstate,mclip) = updateSection state editlensstate newlen;
					} in (newstate,fmap (\clip -> StateLensEdit editlens clip editbedit) mclip) ));
				} `mplus` do
				{
					(MkEqualType,MkEqualType) <- matchTLens (Type :: Type [e]) editlens listElement;
					return (FunctionConstFunction (\olda ->
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
					} in (newstate,fmap (\(clip,_) -> StateLensEdit editlens clip editbedit) mclip) ));					
				};
				_ -> Nothing;
			};
		};
	};
}
