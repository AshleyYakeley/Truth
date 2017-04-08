{-# OPTIONS -fno-warn-orphans #-}
module Truth.UI.GTK.CheckButton where
{
    import Data.Type.Equality;
    import Graphics.UI.Gtk;
    import Data.Reity;
    import Truth.Core;
    import Truth.UI.GTK.GView;
    import Truth.UI.GTK.Useful;


    checkButtonView :: String -> GView (WholeEdit (WholeReader Bool));
    checkButtonView name = MkView $ \(MkLockAPI lapi) -> do
    {
        widget <- checkButtonNew;
        initial <- lapi $ \() api -> unReadable fromReader $ apiRead api;
        set widget [buttonLabel := name,toggleButtonActive := initial];
        clickConnection <- onClicked widget $ lapi $ \() api -> do
        {
            s <- liftIO $ get widget toggleButtonActive;
            _ <- apiEdit api $ getReplaceEdits s;
            return ();
        };

        let
        {
            vrWidgetStuff = MkViewWidgetStuff (toWidget widget) (return Nothing);

            vrUpdate () edit = do
            {
                newstate <- fmap (fromReadFunction (applyEdits edit)) $ get widget toggleButtonActive;
                withSignalBlocked clickConnection $ set widget [toggleButtonActive := newstate];
                return ();
            };
        };
        return (MkViewResult{..},());
    };

    checkButtonMatchView :: MatchView;
    checkButtonMatchView tedit = do
    {
        Refl <- testEquality (info :: Info (WholeEdit (WholeReader Bool))) tedit;
        return $ checkButtonView "";
    };
}
