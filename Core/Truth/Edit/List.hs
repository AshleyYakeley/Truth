{-# OPTIONS_GHC -fno-warn-orphans #-}
module Truth.Edit.List(listElement,listSection,ListEdit(..)) where
{
    import Truth.Edit.IndexEdit;
    import Truth.Edit.FloatingEditLens;
    import Truth.Edit.JustWholeEdit;
    import Truth.Edit.JustEdit;
    import Truth.Edit.Edit;
    import Truth.Edit.Import;

    data ListEdit edit =
        ReplaceListEdit [Subject edit]                  |
        ItemEdit (IndexEdit [Subject edit] Int edit)    |
        ReplaceSectionEdit (Int,Int) [Subject edit]     ;

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

    instance HasInfoKTT ListEdit where
    {
        infoKTT = MkInfoKTT
            (WitKTT (unsafeIOWitnessFromString "Truth.Edit.List.ListEdit"))
            (
                (mkTFactsKTT (\tedit -> do
                    {
                        MkEditInst tsubj <- matchPropertyT tedit;
                        return (MkEditInst (applyTInfoT (infoKTT :: InfoKTT []) tsubj));
                    })
                ) `mappend`
                (mkTFactsKTT (\tedit -> do
                    {
                        MkEditInst _ <- matchPropertyT tedit;
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


    listElement :: forall edit. (HasNewValue (Subject edit),FullEdit edit) => Int -> FloatingEditLens Int (ListEdit edit) (JustWholeEdit Maybe edit);
    listElement initial = MkFloatingEditLens
    {
        floatingEditLensUpdate = elementUpdate,
        floatingEditLensSimple = MkFloatingLens
        {
            floatingLensInitial = initial,
            floatingLensGet = \i -> lensGet (indexLens i),
            -- :: state -> a -> b,
            floatingLensPutback = \i b -> do
            {
                ma <- lensPutback (indexLens i) b;
                return (fmap (\a -> (i,a)) ma);
            }
            -- :: state -> b -> ConstFunction a (m (state,a))
        },
        floatingEditLensPutEdit = \i eme -> pure (do
        {
            ee <- extractJustWholeEdit eme;
            return (i,ItemEdit (MkIndexEdit i ee));
        })
    } where
    {
        listElement' :: Int -> FloatingEditLens Int (ListEdit edit) (JustWholeEdit Maybe edit);
        listElement' = listElement;

        elementUpdate :: ListEdit edit -> Int -> ConstFunction [Subject edit] (Int,Maybe (JustWholeEdit Maybe edit));
        elementUpdate edita state =
          fromMaybe (fmap (\newa -> (state,Just (replaceEdit (floatingEditLensGet (listElement' state) state newa)))) (applyEdit edita)) update_ where
        {
            update_ :: Maybe (ConstFunction [Subject edit] (Int,Maybe (JustWholeEdit Maybe edit)));
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

    listSection :: forall edit. (HasNewValue (Subject edit),FullEdit edit) => (Int,Int) -> FloatingEditLens (Int,Int) (ListEdit edit) (ListEdit edit);
    listSection initial = MkFloatingEditLens
    {
        floatingEditLensUpdate = sectionUpdate,
        floatingEditLensSimple = MkFloatingLens
        {
            floatingLensInitial = initial,
            floatingLensGet = \(start,len) list -> take len (drop start list),
            floatingLensPutback = \(start,len) section -> arr (\list -> Just ((start,length section),(take start list) ++ section ++ (drop (start + len) list)))
        },
        floatingEditLensPutEdit = \clip@(start,clen) ele -> pure (Just (case ele of
        {
            ReplaceListEdit newlist -> ((start,length newlist),ReplaceSectionEdit clip newlist);
            ItemEdit (MkIndexEdit i edit) -> (clip,ItemEdit (MkIndexEdit (start + i) edit));
            ReplaceSectionEdit (s,len) newlist -> ((start,clen + (length newlist) - len),ReplaceSectionEdit (start + s,len) newlist);
        }))
    } where
    {
        listSection' :: (Int,Int) -> FloatingEditLens (Int,Int) (ListEdit edit) (ListEdit edit);
        listSection' = listSection;

        listElement' :: Int -> FloatingEditLens Int (ListEdit edit) (JustWholeEdit Maybe edit);
        listElement' = listElement;

        sectionUpdate :: ListEdit edit -> (Int,Int) -> ConstFunction [Subject edit] ((Int,Int),Maybe (ListEdit edit));
        sectionUpdate edita state =
          fromMaybe (fmap (\newa -> (state,Just (replaceEdit (floatingEditLensGet (listSection' state) state newa)))) (applyEdit edita)) update_ where
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
                    oldb = floatingEditLensGet (listElement' editlensstate) editlensstate olda;
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
