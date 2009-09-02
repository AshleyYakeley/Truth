{-# LANGUAGE ViewPatterns, FlexibleContexts #-}
module UI.Truth.GTK.Window where
{
    --import UI.Truth.GTK.Text;
    import UI.Truth.GTK.Maybe;
    import UI.Truth.GTK.GView;
    import Graphics.UI.Gtk;
    import Data.Changes;
    import Data.OpenWitness.OpenRep;
    import Data.Witness;
    import Data.IORef;

    makeButton :: String -> IO () -> IO Button;
    makeButton name action = do
    {
        button <- buttonNew;
        set button [buttonLabel := name];
        onClicked button action;
        return button;
    };

    lastResortView :: GView a edit;
    lastResortView = undefined;

    data MatchRep1 f p where
    {
        MkMatchRep1 :: forall a. EqualType (f a) p -> OpenRep a -> MatchRep1 f p;
    };

    matchRep1 :: forall f p. OpenRep1 f -> OpenRep p -> Maybe (MatchRep1 f p);
    matchRep1 repf1 (ApplyOpenRep repf2 repa) = do
    {
        MkEqualType <- matchOpenRep1 repf1 repf2;
        return (MkMatchRep1 MkEqualType repa);
    };
    matchRep1 _ _ = Nothing;

    data MatchRep2 f p where
    {
        MkMatchRep2 :: forall a. EqualType (f a b) p -> OpenRep a -> OpenRep b -> MatchRep2 f p;
    };

    matchRep2 :: forall f p. OpenRep2 f -> OpenRep p -> Maybe (MatchRep2 f p);
    matchRep2 repf1 (ApplyOpenRep (ApplyOpenRep1 repf2 repa) repb) = do
    {
        MkEqualType <- matchOpenRep2 repf1 repf2;
        return (MkMatchRep2 MkEqualType repa repb);
    };
    matchRep2 _ _ = Nothing;

    type MatchView = forall a edit. (CompleteEditScheme a edit,HasNewValue a) => OpenRep a -> OpenRep edit -> Maybe (GView a edit);

    theView' :: forall a edit. (CompleteEditScheme a edit,HasNewValue a) => OpenRep a -> OpenRep edit -> GView (Maybe a) (JustEdit (Maybe a) edit);
    theView' repa repedit = maybeView newValue (theView matchViews repa repedit);
{-
    problem :: (HasNewValue (Maybe a)) => Type a -> ((HasNewValue a) => r) -> r;
    problem _ r = r;

    maybeMatchView :: forall a edit. (CompleteEditScheme a edit,HasNewValue a) => OpenRep a -> OpenRep edit -> Maybe (GView a edit);
    maybeMatchView repb repeditb = do
    {
        (MkMatchRep1 MkEqualType repa) <- matchRep1 (typeRep1 :: OpenRep1 Maybe) repb;
        (MkMatchRep2 MkEqualType repma repedit) <- matchRep2 (typeRep2 :: OpenRep2 JustEdit) repeditb;
        (MkMatchRep1 MkEqualType repa') <- matchRep1 (typeRep1 :: OpenRep1 Maybe) repma;
        MkEqualType <- matchWitness repa repa';
        return (problem (Type :: (Maybe x ~ a) => Type x) (theView' repa repedit));
    };
-}
    matchViews :: [MatchView];
--    matchViews = [maybeMatchView];
    matchViews = [];

    theView :: forall a edit. (CompleteEditScheme a edit,HasNewValue a) => [MatchView] -> OpenRep a -> OpenRep edit -> GView a edit;
    theView [] _ _ = lastResortView;
    theView (mview:mviews) edita editrep = case mview edita editrep of
    {
        Just gview -> gview;
        _ -> theView mviews edita editrep;
    };
    
    
    
{-
--    theView (ApplyOpenRep rep1 rep) | Just MkEqualType <- matchOpenRep1 (typeRep1 :: OpenRep1 Maybe) rep1 = maybeView newValue (theView rep);
    theView (ApplyOpenRep repa1 repa) (ApplyOpenRep (ApplyOpenRep1 repedit2 _repedite) repedit) =
     case (matchOpenRep1 (typeRep1 :: OpenRep1 Maybe) repa1,matchOpenRep2 (typeRep2 :: OpenRep2 JustEdit) repedit2) of
    {
        (Just MkEqualType,Just MkEqualType) -> theView' repa repedit;
        _ -> lastResortView;
    };
    theView _ _ = lastResortView;
-}
    makeWindow :: (HasTypeRep a, HasTypeRep edit, CompleteEditScheme a edit,HasNewValue a) => IORef Int -> IO () -> Subscribe a edit -> IO ();
    makeWindow ref tellclose sub = do
    {
        (sub',w,close) <- subscribeView (theView matchViews typeRep typeRep) sub;
        window <- windowNew;
        _selectionButton <- makeButton "Selection" (do
        {
            msel <- wsGetSelection w;
            case msel of
            {
                Just (MkSelection lens state) -> do
                {
                    makeWindowCountRef ref (lensSubscribe lens state sub');
                };
                _ -> return ();
            };
        });
        set window [containerChild := wsWidget w];
        widgetShow (wsWidget w);
        onDestroy window (do
        {
            close;
            tellclose;
        });
        widgetShow window;
        return ();
    };

    makeWindowCountRef :: (HasTypeRep a, HasTypeRep edit, CompleteEditScheme a edit,HasNewValue a) => IORef Int -> Subscribe a edit -> IO ();
    makeWindowCountRef windowCount sub = do
    {
        makeWindow windowCount (do
        {
            i <- readIORef windowCount;
            writeIORef windowCount (i - 1);
            if i == 1
             then mainQuit
             else return ();
        }) sub;
        i <- readIORef windowCount;
        writeIORef windowCount (i + 1);
        return ();
    };
}
