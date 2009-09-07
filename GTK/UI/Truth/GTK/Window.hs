{-# LANGUAGE ViewPatterns, FlexibleContexts #-}
module UI.Truth.GTK.Window where
{
    import UI.Truth.GTK.Text;
    import UI.Truth.GTK.CheckButton;
    import UI.Truth.GTK.Maybe;
    import UI.Truth.GTK.GView;
    import Graphics.UI.Gtk;
    import Data.Changes;
    import Data.Result;
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
    lastResortView = \_ _ -> do
    {
        w <- labelNew (Just "Uneditable");
        return (MkViewResult (MkViewWidgetStuff (toWidget w) (return Nothing)) (\_ -> return ()));
    };

    type MatchView = forall edit. (Edit edit,HasNewValue (Subject edit)) => EditRepT edit -> Maybe (GView edit);

    maybeMatchView :: MatchView;
    maybeMatchView = foo MkEditInst where
    {
        foo :: forall edit. EditInst edit -> EditRepT edit -> Maybe (GView edit);
        foo inst (TEditRepT repJM rep) = do
        {
            MkEqualType <- matchEditRepKTT repJM (typeRepKTT :: EditRepKTT (JustEdit Maybe));
            case inst of
            {
                MkEditInst -> case editEvidence (Type :: Type edit) of
                {
                    (MkHasNewValueInst,_,MkEditInst) -> return (maybeView newValue (theView matchViews rep));
                };
            };
        };
        foo _ _ = Nothing;
    };

    resultMatchView :: MatchView;
    resultMatchView = foo MkEditInst where
    {
        foo :: forall edit. EditInst edit -> EditRepT edit -> Maybe (GView edit);
        foo inst (TEditRepT (KTTEditRepKTT repJustEdit (TEditRepKTT repResult reperr)) rep) = do
        {
            MkEqualType <- matchEditRepKKTTKTT repJustEdit (typeRepKKTTKTT :: EditRepKKTTKTT JustEdit);
            MkEqualType <- matchEditRepKTKTT repResult (typeRepKTKTT :: EditRepKTKTT Result);
            case inst of
            {
                MkEditInst -> case editEvidence (Type :: Type edit) of
                {
                    (MkHasNewValueInst,_,MkEditInst) -> return (resultView reperr (theView matchViews rep));
                };
            };
        };
        foo _ _ = Nothing;
    };

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
    matchViews = [checkButtonMatchView,textMatchView,maybeMatchView,resultMatchView];

    theView :: forall edit. (Edit edit,HasNewValue (Subject edit)) => [MatchView] -> EditRepT edit -> GView edit;
    theView [] _ = lastResortView;
    theView (mview:mviews) editrep = case mview editrep of
    {
        Just gview -> gview;
        _ -> theView mviews editrep;
    };
    
    makeWindow :: (Edit edit, HasNewValue (Subject edit)) => EditRepT edit -> IORef Int -> IO () -> Subscribe edit -> IO ();
    makeWindow rep ref tellclose sub = do
    {
        (sub',w,close) <- subscribeView (theView matchViews rep) sub;
        window <- windowNew;
        box <- vBoxNew False 0;
        
        selectionButton <- makeButton "Selection" (do
        {
            msel <- vwsGetSelection w;
            case msel of
            {
                Just (MkSelection repsel lens state) -> do
                {
                    makeWindowCountRef repsel ref (lensSubscribe lens state sub');
                };
                _ -> return ();
            };
        });
        
        boxPackStart box selectionButton PackNatural 0;
        boxPackStart box (vwsWidget w) PackGrow 0;
        
        set window [containerChild := box];
        widgetShow (vwsWidget w);
        onDestroy window (do
        {
            close;
            tellclose;
        });
        widgetShowAll window;
        return ();
    };

    makeWindowCountRef :: (Edit edit, HasNewValue (Subject edit)) => EditRepT edit -> IORef Int -> Subscribe edit -> IO ();
    makeWindowCountRef rep windowCount sub = do
    {
        makeWindow rep windowCount (do
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
