{-# OPTIONS -fno-warn-orphans #-}
module Truth.UI.GTK.CheckButton where
{
    import Data.Type.Equality;
    import Data.Empty;
    import Graphics.UI.Gtk;
    import Data.Reity;
    import Truth.Core;
    import Truth.UI.GTK.GView;
    import Truth.UI.GTK.Useful;


    checkButtonView :: String -> GView (WholeEdit (WholeReader Bool));
    checkButtonView name = MkView $ \(MkLockAPI lapi) _setSelect -> do
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
            vrWidget = toWidget widget;
            vrFirstUpdateState = ();
            vrUpdate () edits = do
            {
                newstate <- fmap (fromReadFunction (applyEdits edits)) $ get widget toggleButtonActive;
                withSignalBlocked clickConnection $ set widget [toggleButtonActive := newstate];
                return ();
            };
            vrFirstSelState = Nothing;
            vrGetSelection (ss :: None) = never ss;
        };
        return MkViewResult{..};
    };

    checkButtonMatchView :: MatchView;
    checkButtonMatchView tedit = do
    {
        Refl <- testEquality (info :: Info (WholeEdit (WholeReader Bool))) tedit;
        return $ checkButtonView "";
    };
}
