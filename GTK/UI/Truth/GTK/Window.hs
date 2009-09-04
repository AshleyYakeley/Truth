{-# LANGUAGE ViewPatterns, FlexibleContexts #-}
module UI.Truth.GTK.Window where
{
    import UI.Truth.GTK.Text;
    import UI.Truth.GTK.CheckButton;
    --import UI.Truth.GTK.Maybe;
    import UI.Truth.GTK.GView;
    import Graphics.UI.Gtk;
    import Data.Changes;
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

    lastResortView :: GView edit;
    lastResortView = undefined;

    type MatchView = forall edit. (Edit edit,HasNewValue (Subject edit)) => EditRepT edit -> Maybe (GView edit);
{-
    maybeMatchView :: forall edit. (Edit edit,HasNewValue (Subject edit)) => EditRepT edit -> Maybe (GView edit);
    maybeMatchView (TEditRepT repJM rep) = do
    {
        MkEqualType <- matchEditRepKTT repJM (typeRepKTT :: EditRepKTT (JustEdit Maybe));
        return (maybeView newValue (theView matchViews rep));
    };
    maybeMatchView _ = Nothing;
-}
    checkButtonMatchView :: MatchView;
    checkButtonMatchView rep = do
    {
        MkEqualType <- matchWitness rep (typeRepT :: EditRepT (WholeEdit Bool));
        return (checkButtonView "");
    };

    textMatchView :: MatchView;
    textMatchView rep = do
    {
        MkEqualType <- matchWitness rep (typeRepT :: EditRepT (ListEdit (WholeEdit Char)));
        return textView;
    };

    matchViews :: [MatchView];
    matchViews = [checkButtonMatchView,textMatchView{-,maybeMatchView-}];

    theView :: forall edit. (Edit edit,HasNewValue (Subject edit)) => [MatchView] -> EditRepT edit -> GView edit;
    theView [] _ = lastResortView;
    theView (mview:mviews) editrep = case mview editrep of
    {
        Just gview -> gview;
        _ -> theView mviews editrep;
    };
    
    makeWindow :: (HasTypeRepT edit, Edit edit, HasNewValue (Subject edit)) => IORef Int -> IO () -> Subscribe edit -> IO ();
    makeWindow ref tellclose sub = do
    {
        (sub',w,close) <- subscribeView (theView matchViews typeRepT) sub;
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

    makeWindowCountRef :: (HasTypeRepT edit, Edit edit, HasNewValue (Subject edit)) => IORef Int -> Subscribe edit -> IO ();
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
