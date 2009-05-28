{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Changes.List(listElement,listSection,ListPartEdit(..)) where
{
	import Data.Changes.Edit;
	import Control.Arrow;
	import Data.ConstFunction;
	import Data.Maybe;
	import Control.Category;
	import Control.Applicative;
	import Prelude hiding (id,(.));

	elementModify :: Int -> (e -> e) -> [e] -> [e];
	elementModify 0 f (e:es) = (f e):es;
	elementModify _ _ [] = [];
	elementModify i f (e:es) = e:(elementModify (i - 1) f es);

	elementGet :: Int -> [e] -> Maybe e;
	elementGet 0 (e:_) = Just e;
	elementGet _ [] = Nothing;
	elementGet i (_:es) = elementGet (i - 1) es;
{-		
	elementPutback :: Int -> e -> [e] -> Maybe [e];
	elementPutback _ _ [] = Nothing;
	elementPutback 0 x (_:es) = Just (x:es);
	elementPutback i x (e:es) = do
	{
		xs <- elementPutback (i - 1) x es;
		return (e:xs);
	};
-}

	data ListPartEdit a =  ItemEdit Int (Edit a) | ReplaceSectionEdit (Int,Int) [a];

	instance (Editable a) => Editable [a] where
	{
		type PartEdit [a] = ListPartEdit a;
	};

	instance (Editable a) => EditScheme (ListPartEdit a) [a] where
	{
		applyPartEdit (ItemEdit i _) | i < 0 = id;
		applyPartEdit (ItemEdit i edita) = arr (elementModify i (applyConstFunction (applyEditCF edita))) where
		{
		};

		applyPartEdit (ReplaceSectionEdit (start,_) _) | start < 0 = id;
		applyPartEdit (ReplaceSectionEdit (start,len) newmiddle) = arr (\list -> let
		{
			(before,rest) = splitAt start list;
			(_,after) = splitAt len rest;
		} in before ++ newmiddle ++ after);

		invertPartEdit (ItemEdit i _) _ | i < 0 = Nothing;
		invertPartEdit (ItemEdit i edita) oldlist = do
		{
			oldelement <- elementGet i oldlist;
			invedita <- invertEditCF edita oldelement;
			return (PartEdit (ItemEdit i invedita));
		} where
		{
		};
		
		invertPartEdit (ReplaceSectionEdit (start,_) _) _ | start < 0 = Nothing;
		invertPartEdit (ReplaceSectionEdit (start,len) newmiddle) oldlist = Just (PartEdit (ReplaceSectionEdit (start,length newmiddle) oldmiddle)) where
		{
			(_,rest) = splitAt start oldlist;
			(oldmiddle,_) = splitAt len rest;
		};
	};

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
		

	listElement :: forall e. (Editable e) => FloatingLens Int [e] (Maybe e);
	listElement = MkFloatingLens
	{
		lensUpdate = elementUpdate,
		lensGet = \i list -> if i < 0 then Nothing else elementGet i list,
		lensPutEdit = \i eme -> pure (do
		{
			ee <- extractJustEdit eme;
			return (PartEdit (ItemEdit i ee));
		})
	} where
	{
		elementUpdate :: (Editable e) => Edit [e] -> Int -> ConstFunction [e] (Int,Maybe (Edit (Maybe e)));
		elementUpdate edita state = 
		  fromMaybe (fmap (\newa -> (state,Just (ReplaceEdit (lensGet listElement state newa)))) (applyEditCF edita)) update_ where
		{
			update_ :: Maybe (ConstFunction [e] (Int,Maybe (Edit (Maybe e))));
			update_ = case edita of
			{
				PartEdit (ReplaceSectionEdit editlensstate newb) -> Just (pure (let
				{
					newlen = length newb;
					((newstate,_),mclip) = updateSection (state,1) editlensstate newlen;
				} in (newstate,fmap (\_ -> ReplaceEdit (elementGet newstate newb)) mclip)
				));

				PartEdit (ItemEdit editlensstate editbedit) -> Just (pure
					(state,if (editlensstate == state) then Just (PartEdit (JustEdit editbedit)) else Nothing)
				);

				_ -> Nothing;
			};
		};
	};

	listSection :: forall e. (Editable e) => FloatingLens (Int,Int) [e] [e];
	listSection = MkFloatingLens
	{
		lensUpdate = sectionUpdate,
		lensGet = \(start,len) list -> take len (drop start list),
		lensPutEdit = \clip@(start,_) ele -> pure (Just (case ele of
		{
			ReplaceEdit newlist -> PartEdit (ReplaceSectionEdit clip newlist);
			PartEdit (ItemEdit i edit) -> PartEdit (ItemEdit (start + i) edit);
			PartEdit (ReplaceSectionEdit (s,len) newlist) -> PartEdit (ReplaceSectionEdit (start + s,len) newlist);
		}))
	} where
	{
		sectionUpdate :: (Editable e) => Edit [e] -> (Int,Int) -> ConstFunction [e] ((Int,Int),Maybe (Edit [e]));
		sectionUpdate edita state = 
		  fromMaybe (fmap (\newa -> (state,Just (ReplaceEdit (lensGet listSection state newa)))) (applyEditCF edita)) update_ where
		{
			update_ :: Maybe (ConstFunction [e] ((Int,Int),Maybe (Edit [e])));
			update_ = case edita of
			{
				PartEdit (ReplaceSectionEdit editlensstate newb) -> Just (pure (let
				{
					newlen = length newb;
					(newstate,mclip) = updateSection state editlensstate newlen;
				} in (newstate,fmap (\clip -> PartEdit (ReplaceSectionEdit clip newb)) mclip)
				));

				PartEdit (ItemEdit editlensstate editbedit) -> Just (FunctionConstFunction (\olda ->
				let
				{
					oldb = lensGet listElement editlensstate olda;
					newb = applyConstFunctionA (applyEditCF editbedit) oldb;
					newlen = case newb of
					{
						Just _ -> 1;
						_ -> 0;
					};
					(newstate,mclip) = updateSection state (editlensstate,1) newlen;
				} in (newstate,fmap (\(clip,_) -> PartEdit (ItemEdit clip editbedit)) mclip) ));

				_ -> Nothing;
			};
		};
	};
}
