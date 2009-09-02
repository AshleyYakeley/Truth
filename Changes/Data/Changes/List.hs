{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Changes.List(listElement,listSection,ListEdit(..)) where
{
    import Data.Changes.FloatingLens;
    import Data.Changes.JustEdit;
    import Data.Changes.EditScheme;
    import Data.Changes.HasTypeRep;
    import Control.Arrow;
    import Data.ConstFunction;
    import Data.OpenWitness.OpenRep;
    import Data.OpenWitness;
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

--    data FixedListEdit edit = MkFixedListEdit Int edit;

    data ListEdit la edit =  ReplaceListEdit la | ItemEdit Int edit | ReplaceSectionEdit (Int,Int) la;
    
    instance HasTypeRep2 ListEdit where
    {
        typeRep2 = SimpleOpenRep2 (unsafeIOWitnessFromString "Data.Changes.List.ListEdit");
    };

    instance (EditScheme a edit) => EditScheme [a] (ListEdit [a] edit) where
    {
        applyEdit (ReplaceListEdit a) = pure a;
        applyEdit (ItemEdit i _) | i < 0 = id;
        applyEdit (ItemEdit i edita) = arr (elementModify i (applyConstFunction (applyEdit edita))) where
        {
        };
        applyEdit (ReplaceSectionEdit (start,_) _) | start < 0 = id;
        applyEdit (ReplaceSectionEdit (start,len) newmiddle) = arr (\list -> let
        {
            (before,rest) = splitAt start list;
            (_,after) = splitAt len rest;
        } in before ++ newmiddle ++ after);

        invertEdit (ReplaceListEdit _) a = Just (ReplaceListEdit a);
        invertEdit (ItemEdit i _) _ | i < 0 = Nothing;
        invertEdit (ItemEdit i edita) oldlist = do
        {
            oldelement <- elementGet i oldlist;
            invedita <- invertEdit edita oldelement;
            return (ItemEdit i invedita);
        } where
        {
        };        
        invertEdit (ReplaceSectionEdit (start,_) _) _ | start < 0 = Nothing;
        invertEdit (ReplaceSectionEdit (start,len) newmiddle) oldlist = Just (ReplaceSectionEdit (start,length newmiddle) oldmiddle) where
        {
            (_,rest) = splitAt start oldlist;
            (oldmiddle,_) = splitAt len rest;
        };
    };

    instance (EditScheme a edit) => CompleteEditScheme [a] (ListEdit [a] edit) where
    {
        replaceEdit = ReplaceListEdit;
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
       then if editend > end    -- clip start
         then ((editstart,newlen),Just (0,len)) -- clip both
         else ((editstart,end-editend+newlen),Just (0,editend-start)) -- clip start, no clip end
       else if editend > end    -- no clip start
         then ((start,editstart-start+newlen),Just (editstart-start,len-editstart+start)) -- no clip start, clip end
         else ((start,len+newlen-editlen),Just (editstart-start,editlen)); -- within
        

    listElement :: forall e edit. (CompleteEditScheme e edit) => FloatingLens Int [e] (ListEdit [e] edit) (Maybe e) (JustEdit (Maybe e) edit);
    listElement = MkFloatingLens
    {
        lensUpdate = elementUpdate,
        lensGet = \i list -> if i < 0 then Nothing else elementGet i list,
        lensPutEdit = \i eme -> pure (do
        {
            ee <- extractJustEdit eme;
            return (ItemEdit i ee);
        })
    } where
    {
        listElement' :: FloatingLens Int [e] (ListEdit [e] edit) (Maybe e) (JustEdit (Maybe e) edit);
        listElement' = listElement;
    
        elementUpdate :: ListEdit [e] edit -> Int -> ConstFunction [e] (Int,Maybe (JustEdit (Maybe e) edit));
        elementUpdate edita state = 
          fromMaybe (fmap (\newa -> (state,Just (replaceEdit (lensGet listElement' state newa)))) (applyEdit edita)) update_ where
        {
            update_ :: Maybe (ConstFunction [e] (Int,Maybe (JustEdit (Maybe e) edit)));
            update_ = case edita of
            {
                ReplaceSectionEdit editlensstate newb -> Just (pure (let
                {
                    newlen = length newb;
                    ((newstate,_),mclip) = updateSection (state,1) editlensstate newlen;
                } in (newstate,fmap (\_ -> replaceEdit (elementGet newstate newb)) mclip)
                ));

                ItemEdit editlensstate editbedit -> Just (pure
                    (state,if (editlensstate == state) then Just (JustEdit editbedit) else Nothing)
                );

                _ -> Nothing;
            };
        };
    };

    listSection :: forall e edit. (CompleteEditScheme e edit) => FloatingLens (Int,Int) [e] (ListEdit [e] edit) [e] (ListEdit [e] edit);
    listSection = MkFloatingLens
    {
        lensUpdate = sectionUpdate,
        lensGet = \(start,len) list -> take len (drop start list),
        lensPutEdit = \clip@(start,_) ele -> pure (Just (case ele of
        {
            ReplaceListEdit newlist -> ReplaceSectionEdit clip newlist;
            ItemEdit i edit -> ItemEdit (start + i) edit;
            ReplaceSectionEdit (s,len) newlist -> ReplaceSectionEdit (start + s,len) newlist;
        }))
    } where
    {
        listSection' :: FloatingLens (Int,Int) [e] (ListEdit [e] edit) [e] (ListEdit [e] edit);
        listSection' = listSection;
    
        listElement' :: FloatingLens Int [e] (ListEdit [e] edit) (Maybe e) (JustEdit (Maybe e) edit);
        listElement' = listElement;
    
        sectionUpdate :: ListEdit [e] edit -> (Int,Int) -> ConstFunction [e] ((Int,Int),Maybe (ListEdit [e] edit));
        sectionUpdate edita state = 
          fromMaybe (fmap (\newa -> (state,Just (replaceEdit (lensGet listSection' state newa)))) (applyEdit edita)) update_ where
        {
            update_ :: Maybe (ConstFunction [e] ((Int,Int),Maybe (ListEdit [e] edit)));
            update_ = case edita of
            {
                ReplaceSectionEdit editlensstate newb -> Just (pure (let
                {
                    newlen = length newb;
                    (newstate,mclip) = updateSection state editlensstate newlen;
                } in (newstate,fmap (\clip -> ReplaceSectionEdit clip newb) mclip)
                ));

                ItemEdit editlensstate editbedit -> Just (FunctionConstFunction (\olda ->
                let
                {
                    oldb = lensGet listElement' editlensstate olda;
                    newb = applyConstFunctionA (applyEdit editbedit) oldb;
                    newlen = case newb of
                    {
                        Just _ -> 1;
                        _ -> 0;
                    };
                    (newstate,mclip) = updateSection state (editlensstate,1) newlen;
                } in (newstate,fmap (\(clip,_) -> ItemEdit clip editbedit) mclip) ));

                _ -> Nothing;
            };
        };
    };
}
