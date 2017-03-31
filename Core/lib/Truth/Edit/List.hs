{-# OPTIONS_GHC -fno-warn-orphans #-}
module Truth.Edit.List({- listElement,listTake,listDrop,listSection,ListEdit(..),ListPoint,ListRegion(..) -}) where
{
{-
    import Truth.Edit.Import;
    import Truth.Edit.Edit;
    import Truth.Edit.JustEdit;
    import Truth.Edit.JustWholeEdit;
    import Truth.Edit.FloatingEditFunction;
    import Truth.Edit.FloatingEditLens;
    import Truth.Edit.IndexEdit;


    type ListPoint = Int;
    data ListRegion = MkListRegion Int Int deriving (Eq);

    data ListRead a t where
    {
        ListReadLength :: ListRead a ListPoint;
        ListReadSection :: ListRegion -> ListRead a [a];
    };

    data ListEdit edit =
        ReplaceListEdit [ReaderSubject edit]                  |
        ItemEdit (IndexEdit [ReaderSubject edit] edit)    |
        ReplaceSectionEdit ListRegion [ReaderSubject edit]     ;

    instance (Edit edit) => Edit (ListEdit edit) where
    {
        type ReaderSubject (ListEdit edit) = [ReaderSubject edit];

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

    listElement :: forall edit. (HasNewValue (ReaderSubject edit),FullEdit edit) => ListPoint -> FloatingEditLens ListPoint (ListEdit edit) (JustWholeEdit Maybe edit);
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

        elementUpdate :: ListEdit edit -> ListPoint -> ConstFunction [ReaderSubject edit] (ListPoint,Maybe (JustWholeEdit Maybe edit));
        elementUpdate edita state =
          fromMaybe (fmap (\newa -> (state,Just (replaceEdit (floatingEditGet (floatingEditLensFunction (listElement' state)) state newa)))) (applyEdit edita)) update_ where
        {
            update_ :: Maybe (ConstFunction [ReaderSubject edit] (ListPoint,Maybe (JustWholeEdit Maybe edit)));
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

    listTake :: forall edit. ListPoint -> FloatingEditLens ListPoint (ListEdit edit) (ListEdit edit);
    listTake initial = MkFloatingEditLens
    {
        floatingEditLensFunction = MkFloatingEditFunction
        {
            floatingEditInitial = initial,
            floatingEditGet = \mark list -> take mark list,
            floatingEditUpdate = \edita oldmark -> case edita of
            {
                ReplaceListEdit list -> do
                {
                    olda <- id;
                    return (if (oldmark == 0) && ((length olda) > 0) then (oldmark,Nothing) else (length list, Just edita));
                };
                ItemEdit (MkIndexEdit item _editb) -> return (oldmark,if item < oldmark
                 then Just edita
                 else Nothing);
                ReplaceSectionEdit (MkListRegion s len) sec -> return (if s <= oldmark
                 then let
                 {
                    newmark = oldmark + (length sec) - len;
                 }
                 in (newmark,Just (if s + len <= oldmark
                   then edita  -- in lens part
                   else ReplaceSectionEdit (MkListRegion s (oldmark - s)) (take (newmark - s) sec) -- partial
                    ))
                 else (oldmark,Nothing)); -- in ignored part
            }
        },
        floatingEditLensPutEdit = \mark editb -> return (return (case editb of
        {
            ReplaceListEdit newlist -> ReplaceSectionEdit (MkListRegion 0 mark) newlist;
            ItemEdit _ -> editb;
            ReplaceSectionEdit _ _ -> editb;
        }))
    };

    listDrop :: forall edit. ListPoint -> FloatingEditLens ListPoint (ListEdit edit) (ListEdit edit);
    listDrop initial = MkFloatingEditLens
    {
        floatingEditLensFunction = MkFloatingEditFunction
        {
            floatingEditInitial = initial,
            floatingEditGet = \mark list -> drop mark list,
            floatingEditUpdate = \edita oldmark -> case edita of
            {
                ReplaceListEdit _list -> do
                {
                    olda <- id;
                    return
                     (if (oldmark == (length olda)) && ((length olda) > 0)
                      then (oldmark,Nothing)
                      else (0, Just edita) );
                };
                ItemEdit (MkIndexEdit item editb) -> return (oldmark,if item >= oldmark
                 then Just (ItemEdit (MkIndexEdit (item - oldmark) editb))
                 else Nothing);
                ReplaceSectionEdit (MkListRegion s len) sec -> return (if s + len >= oldmark
                 then (oldmark,Just (if s >= oldmark
                   then ReplaceSectionEdit (MkListRegion (s - oldmark) len) sec  -- in lens part
                   else ReplaceSectionEdit (MkListRegion 0 (s - oldmark + len)) (drop (oldmark - s) sec) -- partial
                    ))
                 else (oldmark + (length sec) - len,Nothing) ); -- in ignored part
            }
        },
        floatingEditLensPutEdit = \mark editb -> case editb of
        {
            ReplaceListEdit newlist -> do
            {
                olda <- id;
                return (return (ReplaceSectionEdit (MkListRegion mark ((length olda) - mark)) newlist));
            };
            ItemEdit (MkIndexEdit i edit) -> return (return (ItemEdit (MkIndexEdit (mark + i) edit)));
            ReplaceSectionEdit (MkListRegion s len) newlist -> return (return (ReplaceSectionEdit (MkListRegion (mark + s) len) newlist));
        }
    };

    listSection :: forall edit. (HasNewValue (ReaderSubject edit),FullEdit edit) => ListRegion -> FloatingEditLens (ListPoint,ListPoint) (ListEdit edit) (ListEdit edit);
    listSection (MkListRegion start len) = composeFloating (listTake len) (listDrop start);
-}
}
