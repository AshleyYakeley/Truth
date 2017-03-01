{-# OPTIONS -fno-warn-orphans #-}
module Truth.UI.GTK.CheckButton where
{
    import Truth.UI.GTK.GView;
    import Truth.UI.GTK.Useful;
    import Graphics.UI.Gtk;
    import Truth.Object;
    import Truth.Edit;
    import Data.Reity;
    import Data.Type.Heterogeneous;


    checkButtonView :: String -> GView (WholeEdit (WholeReader Bool));
    checkButtonView name = MkView $ \lapi -> do
    {
        widget <- checkButtonNew;
        initial <- lapi $ \() api -> unReadable fromReader $ apiRead api;
        set widget [buttonLabel := name,toggleButtonActive := initial];
        clickConnection <- onClicked widget $ lapi $ \() api -> do
        {
            s <- get widget toggleButtonActive;
            _ <- apiEdit api (replaceEdit s);
            return ();
        };

        let
        {
            vrWidgetStuff = MkViewWidgetStuff (toWidget widget) (return Nothing);

            vrUpdate () edit = do
            {
                newstate <- fmap (fromReadFunction (applyEdit edit)) $ get widget toggleButtonActive;
                withSignalBlocked clickConnection $ set widget [toggleButtonActive := newstate];
                return ();
            };
        };
        return (MkViewResult{..},());
    };

    checkButtonMatchView :: MatchView;
    checkButtonMatchView tedit = do
    {
        -- Refl <- matchProp $(type1[t|(:~:) (WholeEdit Bool)|]) tedit;
        ReflH <- testHetEquality (info :: Info (WholeEdit (WholeReader Bool))) tedit;
        return $ checkButtonView "";
    };
}
