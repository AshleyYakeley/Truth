{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Changes.List(listElement,listSection,ListEdit(..)) where
{
    import Data.Changes.IndexEdit;
    import Data.Changes.FloatingLens;
    import Data.Changes.SimpleLens;
    import Data.Changes.JustEdit;
    import Data.Changes.Edit;
    import Data.TypeKT;
    import Data.Changes.HasNewValue;
    import Control.Arrow;
    import Data.ConstFunction;
    import Data.OpenWitness;
    import Data.Maybe;
    import Control.Category;
    import Control.Applicative;
    import Prelude hiding (id,(.));

    data ListEdit edit = ReplaceListEdit [Subject edit] | ItemEdit (IndexEdit [Subject edit] Int edit) | ReplaceSectionEdit (Int,Int) [Subject edit];

    instance (Edit edit) => Edit (ListEdit edit) where
    {
        type Subject (ListEdit edit) = [Subject edit];

        applyEdit (ReplaceListEdit a) = pure a;
        applyEdit (ItemEdit edit) = applyEdit edit;
        applyEdit (ReplaceSectionEdit (start,_) _) | start < 0 = id;
        applyEdit (ReplaceSectionEdit (start,len) newmiddle) = arr (\list -> let
        {
            (before,rest) = splitAt start list;
            (_,after) = splitAt len rest;
        } in before ++ newmiddle ++ after);

        invertEdit (ReplaceListEdit _) a = Just (ReplaceListEdit a);
        invertEdit (ItemEdit edit) oldlist = fmap ItemEdit (invertEdit edit oldlist);
        invertEdit (ReplaceSectionEdit (start,_) _) _ | start < 0 = Nothing;
        invertEdit (ReplaceSectionEdit (start,len) newmiddle) oldlist = Just (ReplaceSectionEdit (start,length newmiddle) oldmiddle) where
        {
            (_,rest) = splitAt start oldlist;
            (oldmiddle,_) = splitAt len rest;
        };
    };

    instance (Edit edit) => FullEdit (ListEdit edit) where
    {
        replaceEdit = ReplaceListEdit;
    };

    instance HasTypeKTT ListEdit where
    {
        typeKTT = MkTypeKTT
            (WitKTT (unsafeIOWitnessFromString "Data.Changes.List.ListEdit"))
            (
                (mkTInfoKTT (\tedit -> do
                    {
                        MkEditInst tsubj <- typeFactT tedit;
                        return (MkEditInst (applyTTypeT (typeKTT :: TypeKTT []) tsubj));
                    })
                ) `mappend`
                (mkTInfoKTT (\tedit -> do
                    {
                        MkEditInst _ <- typeFactT tedit;
                        return MkFullEditInst;
                    }))
            );
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


    listElement :: forall edit. (HasNewValue (Subject edit),FullEdit edit) => Int -> FloatingLens Int (ListEdit edit) (JustRepEdit Maybe edit);
    listElement initial = MkFloatingLens
    {
        lensInitial = initial,
        lensUpdate = elementUpdate,
        lensSimple = \i -> indexLens i,
        lensPutEdit = \i eme -> pure (do
        {
            ee <- extractJustEdit eme;
            return (ItemEdit (MkIndexEdit i ee));
        })
    } where
    {
        listElement' :: Int -> FloatingLens Int (ListEdit edit) (JustRepEdit Maybe edit);
        listElement' = listElement;

        elementUpdate :: ListEdit edit -> Int -> ConstFunction [Subject edit] (Int,Maybe (JustRepEdit Maybe edit));
        elementUpdate edita state =
          fromMaybe (fmap (\newa -> (state,Just (replaceEdit (lensGet (listElement' state) state newa)))) (applyEdit edita)) update_ where
        {
            update_ :: Maybe (ConstFunction [Subject edit] (Int,Maybe (JustRepEdit Maybe edit)));
            update_ = case edita of
            {
                ReplaceSectionEdit editlensstate newb -> Just (pure (let
                {
                    newlen = length newb;
                    ((newstate,_),mclip) = updateSection (state,1) editlensstate newlen;
                } in (newstate,fmap (\_ -> replaceEdit (elementGet newstate newb)) mclip)
                ));

                ItemEdit (MkIndexEdit editlensstate editbedit) -> Just (pure
                    (state,if (editlensstate == state) then Just (Right (MkJustEdit editbedit)) else Nothing)
                );

                _ -> Nothing;
            };
        };
    };

    listSection :: forall edit. (HasNewValue (Subject edit),FullEdit edit) => (Int,Int) -> FloatingLens (Int,Int) (ListEdit edit) (ListEdit edit);
    listSection initial = MkFloatingLens
    {
        lensInitial = initial,
        lensUpdate = sectionUpdate,
        lensSimple = \(start,len) -> MkSimpleLens
        {
            simpleLensGet = \list -> take len (drop start list),
            simpleLensPutback = \section -> arr (\list -> Just ((take start list) ++ section ++ (drop (start + len) list)))
        },
        lensPutEdit = \clip@(start,_) ele -> pure (Just (case ele of
        {
            ReplaceListEdit newlist -> ReplaceSectionEdit clip newlist;
            ItemEdit (MkIndexEdit i edit) -> ItemEdit (MkIndexEdit (start + i) edit);
            ReplaceSectionEdit (s,len) newlist -> ReplaceSectionEdit (start + s,len) newlist;
        }))
    } where
    {
        listSection' :: (Int,Int) -> FloatingLens (Int,Int) (ListEdit edit) (ListEdit edit);
        listSection' = listSection;

        listElement' :: Int -> FloatingLens Int (ListEdit edit) (JustRepEdit Maybe edit);
        listElement' = listElement;

        sectionUpdate :: ListEdit edit -> (Int,Int) -> ConstFunction [Subject edit] ((Int,Int),Maybe (ListEdit edit));
        sectionUpdate edita state =
          fromMaybe (fmap (\newa -> (state,Just (replaceEdit (lensGet (listSection' state) state newa)))) (applyEdit edita)) update_ where
        {
            update_ :: Maybe (ConstFunction [Subject edit] ((Int,Int),Maybe (ListEdit edit)));
            update_ = case edita of
            {
                ReplaceSectionEdit editlensstate newb -> Just (pure (let
                {
                    newlen = length newb;
                    (newstate,mclip) = updateSection state editlensstate newlen;
                } in (newstate,fmap (\clip -> ReplaceSectionEdit clip newb) mclip)
                ));

                ItemEdit (MkIndexEdit editlensstate editbedit) -> Just (FunctionConstFunction (\olda ->
                let
                {
                    oldb = lensGet (listElement' editlensstate) editlensstate olda;
                    newb = applyConstFunctionA (applyEdit editbedit) oldb;
                    newlen = case newb of
                    {
                        Just _ -> 1;
                        _ -> 0;
                    };
                    (newstate,mclip) = updateSection state (editlensstate,1) newlen;
                } in (newstate,fmap (\(clip,_) -> ItemEdit (MkIndexEdit clip editbedit)) mclip) ));

                _ -> Nothing;
            };
        };
    };
}
