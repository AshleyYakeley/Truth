{-# OPTIONS -fno-warn-orphans #-}
module Truth.UI.GTK.CheckButton where
{
    import Data.Type.Equality;
    import Data.Empty;
    import Graphics.UI.Gtk;
    import Control.Monad.IOInvert;
    import Data.Reity;
    import Truth.Core;
    import Truth.UI.GTK.GView;
    import Truth.UI.GTK.Useful;


    checkButtonView :: String -> GView (WholeEdit (WholeReader Bool));
    checkButtonView name = MkView $ \(MkObject object) _setSelect -> do
    {
        widget <- checkButtonNew;
        initial <- object $ \() muted -> unReadable fromReader $ mutableRead muted;
        set widget [buttonLabel := name,toggleButtonActive := initial];
        clickConnection <- onClicked widget $ object $ \() muted -> do
        {
            s <- liftIO $ get widget toggleButtonActive;
            _ <- mutableEdit muted $ getReplaceEdits s;
            return ();
        };

        let
        {
            vrWidget = toWidget widget;
            vrFirstUpdateState = ();
            vrUpdate :: forall m. MonadIOInvert m => MutableRead m (WholeReader Bool) -> () -> [WholeEdit (WholeReader Bool)] -> m ();
            vrUpdate _ () edits = liftIO $ do
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
    checkButtonMatchView = MkMatchView $ \tedit -> do
    {
        Refl <- testEquality (info :: Info (WholeEdit (WholeReader Bool))) tedit;
        return $ checkButtonView "";
    };
}
