{-# OPTIONS_GHC -fno-warn-orphans #-}
module Truth.Edit.List(listElement,listSection,ListEdit(..)) where
{
    import Truth.Edit.IndexEdit;
    import Truth.Edit.FloatingEditLens;
    import Truth.Edit.JustWholeEdit;
    import Truth.Edit.JustEdit;
    import Truth.Edit.Edit;
    import Truth.Edit.Import;

    type ListPoint = Int;
    data ListRegion = MkListRegion Int Int;

    data ListEdit edit =
        ReplaceListEdit [Subject edit]                  |
        ItemEdit (IndexEdit [Subject edit] ListPoint edit)    |
        ReplaceSectionEdit ListRegion [Subject edit]     ;

    instance (Edit edit) => Edit (ListEdit edit) where
    {
        type Subject (ListEdit edit) = [Subject edit];

        applyEdit (ReplaceListEdit a) = pure a;
        applyEdit (ItemEdit edit) = applyEdit edit;
        applyEdit (ReplaceSectionEdit (MkListRegion start _) _) | start < 0 = id;
        applyEdit (ReplaceSectionEdit (MkListRegion start len) newmiddle) = arr (\list -> let
        {
            (before,rest) = splitAt start list;
            (_,after) = splitAt len rest;
        } in before ++ newmiddle ++ after);

        invertEdit (ReplaceListEdit _) a = Just (ReplaceListEdit a);
        invertEdit (ItemEdit edit) oldlist = fmap ItemEdit (invertEdit edit oldlist);
        invertEdit (ReplaceSectionEdit (MkListRegion start _) _) _ | start < 0 = Nothing;
        invertEdit (ReplaceSectionEdit (MkListRegion start len) newmiddle) oldlist =
         Just (ReplaceSectionEdit (MkListRegion start (length newmiddle)) oldmiddle) where
        {
            (_,rest) = splitAt start oldlist;
            (oldmiddle,_) = splitAt len rest;
        };

        updateEdit (ItemEdit _) edit = edit;
        updateEdit (ReplaceListEdit _) edit = edit;
        updateEdit (ReplaceSectionEdit _ _) edit@(ReplaceListEdit _) = edit;
        updateEdit _ edit = edit;
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

    moveListRegion :: Int -> ListRegion -> ListRegion;
    moveListRegion n (MkListRegion start len) = MkListRegion (start + n) len;

    mkListRegion :: Int -> Int -> ListRegion;
    mkListRegion start end = MkListRegion start (end - start);

    intersectListRegion :: ListRegion -> ListRegion -> Maybe ListRegion;
    intersectListRegion a@(MkListRegion start len) b@(MkListRegion editstart editlen) = let
    {
        end = start + len;
        editend = editstart + editlen;
    } in if editend <= start then Nothing
     else if editstart >= end then Nothing
     else Just (if editstart < start
       then if editend > end    -- clip start
         then a -- clip both
         else (MkListRegion start (editend - start)) -- clip start, no clip end
       else if editend > end    -- no clip start
         then (MkListRegion editstart (end - editstart)) -- no clip start, clip end
         else b);  -- within

    relativeListSection :: ListRegion -> ListRegion -> Maybe ListRegion;
    relativeListSection state@(MkListRegion statestart _) edit = do
    {
        r <- intersectListRegion state edit;
        return (moveListRegion (- statestart) r);
    };

    updateSection' :: ListRegion -> ListRegion -> Int -> ListRegion;
    updateSection' state@(MkListRegion statestart statelen) _edit@(MkListRegion editstart editlen) newlen = let
    {
        stateend = statestart + statelen;
        editend = editstart + editlen;
        lendiff = newlen-editlen;
        -- lendiff = neweditend - editend;
        neweditend = editstart + newlen;
    } in if editend == statestart && editstart == stateend then MkListRegion statestart newlen -- point expansion
     else if editend <= statestart then moveListRegion lendiff state -- entirely before
     else if editstart >= stateend then state -- entirely after
     else if editstart < statestart
       then if editend > stateend    -- clip start
         then mkListRegion editstart neweditend -- clip both
         else mkListRegion editstart (stateend + lendiff) -- clip start, no clip end
       else if editend > stateend    -- no clip start
         then mkListRegion statestart neweditend -- no clip start, clip end
         else moveListRegion lendiff state; -- within

    updateSection :: ListRegion -> ListRegion -> Int -> (ListRegion,Maybe ListRegion);
    updateSection state edit newlen = (updateSection' state edit newlen,relativeListSection state edit);

    listElement :: forall edit. (HasNewValue (Subject edit),FullEdit edit) => ListPoint -> FloatingEditLens ListPoint (ListEdit edit) (JustWholeEdit Maybe edit);
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
        listElement' :: ListPoint -> FloatingEditLens ListPoint (ListEdit edit) (JustWholeEdit Maybe edit);
        listElement' = listElement;

        elementUpdate :: ListEdit edit -> ListPoint -> ConstFunction [Subject edit] (ListPoint,Maybe (JustWholeEdit Maybe edit));
        elementUpdate edita state =
          fromMaybe (fmap (\newa -> (state,Just (replaceEdit (floatingEditLensGet (listElement' state) state newa)))) (applyEdit edita)) update_ where
        {
            update_ :: Maybe (ConstFunction [Subject edit] (ListPoint,Maybe (JustWholeEdit Maybe edit)));
            update_ = case edita of
            {
                ReplaceSectionEdit editlensstate newb -> Just (pure (let
                {
                    newlen = length newb;
                    ((MkListRegion newstate _),mclip) = updateSection (MkListRegion state 1) editlensstate newlen;
                } in (newstate,fmap (\_ -> replaceEdit (elementGet newstate newb)) mclip)
                ));

                ItemEdit (MkIndexEdit editlensstate editbedit) -> Just (pure
                    (state,if (editlensstate == state) then Just (Right (MkJustEdit editbedit)) else Nothing)
                );

                _ -> Nothing;
            };
        };
    };

    listSection :: forall edit. (HasNewValue (Subject edit),FullEdit edit) => ListRegion -> FloatingEditLens ListRegion (ListEdit edit) (ListEdit edit);
    listSection initial = MkFloatingEditLens
    {
        floatingEditLensUpdate = sectionUpdate,
        floatingEditLensSimple = MkFloatingLens
        {
            floatingLensInitial = initial,
            floatingLensGet = \(MkListRegion start len) list -> take len (drop start list),
            floatingLensPutback = \(MkListRegion start len) section ->
             arr (\list -> Just ((MkListRegion start (length section)),(take start list) ++ section ++ (drop (start + len) list)))
        },
        floatingEditLensPutEdit = \clip@(MkListRegion start clen) ele -> pure (Just (case ele of
        {
            ReplaceListEdit newlist -> ((MkListRegion start (length newlist)),ReplaceSectionEdit clip newlist);
            ItemEdit (MkIndexEdit i edit) -> (clip,ItemEdit (MkIndexEdit (start + i) edit));
            ReplaceSectionEdit (MkListRegion s len) newlist ->
             ((MkListRegion start (clen + (length newlist) - len)),ReplaceSectionEdit (MkListRegion (start + s) len) newlist);
        }))
    } where
    {
        listSection' :: ListRegion -> FloatingEditLens ListRegion (ListEdit edit) (ListEdit edit);
        listSection' = listSection;

        listElement' :: ListPoint -> FloatingEditLens ListPoint (ListEdit edit) (JustWholeEdit Maybe edit);
        listElement' = listElement;

        sectionUpdate :: ListEdit edit -> ListRegion -> ConstFunction [Subject edit] (ListRegion,Maybe (ListEdit edit));
        sectionUpdate edita state =
          fromMaybe (fmap (\newa -> (state,Just (replaceEdit (floatingEditLensGet (listSection' state) state newa)))) (applyEdit edita)) update_ where
        {
            update_ :: Maybe (ConstFunction [Subject edit] (ListRegion,Maybe (ListEdit edit)));
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
                    (newstate,mclip) = updateSection state (MkListRegion editlensstate 1) newlen;
                } in (newstate,fmap (\(MkListRegion clip _) -> ItemEdit (MkIndexEdit clip editbedit)) mclip) ));

                _ -> Nothing;
            };
        };
    };
}
