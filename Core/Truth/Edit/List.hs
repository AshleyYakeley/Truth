{-# OPTIONS_GHC -fno-warn-orphans #-}
module Truth.Edit.List(listElement,listSection,ListEdit(..),ListPoint,ListRegion(..)) where
{
    import Truth.Edit.IndexEdit;
    import Truth.Edit.FloatingEditLens;
    import Truth.Edit.FloatingEditFunction;
    import Truth.Edit.JustWholeEdit;
    import Truth.Edit.JustEdit;
    import Truth.Edit.Edit;
    import Truth.Edit.Import;

    type ListPoint = Int;
    data ListRegion = MkListRegion Int Int deriving (Eq);

    data ListEdit edit =
        ReplaceListEdit [Subject edit]                  |
        ItemEdit (IndexEdit [Subject edit] edit)    |
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

        -- WRONG
        updateEdit (ItemEdit iedit') (ItemEdit iedit) = ItemEdit (updateEdit iedit' iedit);
        updateEdit (ReplaceListEdit _) edit = edit;
        updateEdit (ReplaceSectionEdit _ _) edit@(ReplaceListEdit _) = edit;
        updateEdit _ edit = edit;
    };

    instance (Edit edit) => FullEdit (ListEdit edit) where
    {
        replaceEdit = ReplaceListEdit;
    };

    instance HasInfo (Type_KTT ListEdit) where
    {
        info = mkSimpleInfo $(iowitness[t| Type_KTT ListEdit |])
        [
            mkFacts (MkFactS (\tedit -> MkFactZ (do
            {
                Edit_Inst tsubj <- matchProp $(type1[t|Edit_Inst|]) tedit;
                return (Edit_Inst (applyInfo (info :: Info (Type_KTT [])) tsubj));
            }))
            :: FactS FactZ Edit_Inst (Type_KTT ListEdit)
            ),
            mkFacts (MkFactS (\tedit -> MkFactZ (do
            {
                Edit_Inst _ <- matchProp $(type1[t|Edit_Inst|]) tedit;
                return (FullEdit_Inst);
            }))
            :: FactS FactZ FullEdit_Inst (Type_KTT ListEdit)
            )
        ];
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
        floatingEditLensFunction = MkFloatingEditFunction
        {
            floatingEditInitial = initial,
            floatingEditGet = \i -> lensGet (indexLens i),
            floatingEditUpdate = elementUpdate
        },
        floatingEditLensPutEdit = \i eme -> pure (do
        {
            ee <- extractJustWholeEdit eme;
            return (ItemEdit (MkIndexEdit i ee));
        })
    } where
    {
        listElement' :: ListPoint -> FloatingEditLens ListPoint (ListEdit edit) (JustWholeEdit Maybe edit);
        listElement' = listElement;

        elementUpdate :: ListEdit edit -> ListPoint -> ConstFunction [Subject edit] (ListPoint,Maybe (JustWholeEdit Maybe edit));
        elementUpdate edita state =
          fromMaybe (fmap (\newa -> (state,Just (replaceEdit (floatingEditGet (floatingEditLensFunction (listElement' state)) state newa)))) (applyEdit edita)) update_ where
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
        floatingEditLensFunction = MkFloatingEditFunction
        {
            floatingEditInitial = initial,
            floatingEditGet = \(MkListRegion start len) list -> take len (drop start list),
            floatingEditUpdate = sectionUpdate
        },
        floatingEditLensPutEdit = \clip@(MkListRegion start _clen) ele -> pure (Just (case ele of
        {
            ReplaceListEdit newlist -> ReplaceSectionEdit clip newlist;
            ItemEdit (MkIndexEdit i edit) -> ItemEdit (MkIndexEdit (start + i) edit);
            ReplaceSectionEdit (MkListRegion s len) newlist -> ReplaceSectionEdit (MkListRegion (start + s) len) newlist;
        }))
    } where
    {
        listSection' :: ListRegion -> FloatingEditLens ListRegion (ListEdit edit) (ListEdit edit);
        listSection' = listSection;

        listElement' :: ListPoint -> FloatingEditLens ListPoint (ListEdit edit) (JustWholeEdit Maybe edit);
        listElement' = listElement;

        sectionUpdate :: ListEdit edit -> ListRegion -> ConstFunction [Subject edit] (ListRegion,Maybe (ListEdit edit));
        sectionUpdate edita state =
          fromMaybe (fmap (\newa -> (state,Just (replaceEdit (floatingEditGet (floatingEditLensFunction (listSection' state)) state newa)))) (applyEdit edita)) update_ where
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
                    oldb = floatingEditGet (floatingEditLensFunction (listElement' editlensstate)) editlensstate olda;
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
