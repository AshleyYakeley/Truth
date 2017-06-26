{-# OPTIONS -fno-warn-orphans #-}
module Truth.UI.GTK.CheckButton where
{
    import Data.Type.Equality;
    import Data.Empty;
    import Control.Monad.Trans.Class;
    import Control.Monad.Trans.State hiding (get);
    import Graphics.UI.Gtk;
    import Control.Monad.IOInvert;
    import Data.Reity;
    import Truth.Core;
    import Truth.UI.GTK.GView;
    import Truth.UI.GTK.Useful;


    checkButtonView :: String -> GView (WholeEdit Bool);
    checkButtonView name = MkView $ \(MkObject object) _setSelect -> do
    {
        widget <- checkButtonNew;
        initial <- object $ \muted -> lift $ unReadable fromReader $ mutableRead muted;
        set widget [buttonLabel := name,toggleButtonActive := initial];
        clickConnection <- onClicked widget $ object $ \muted -> do
        {
            s <- liftIO $ get widget toggleButtonActive;
            _ <- lift $ mutableEdit muted $ getReplaceEdits s;
            return ();
        };

        let
        {
            vrWidget = toWidget widget;
            vrFirstUpdateState = ();
            vrUpdate :: forall m. IsStateIO m => MutableRead m (WholeReader Bool) -> [WholeEdit Bool] -> StateT () m ();
            vrUpdate _ edits = liftIO $ do
            {
                newstate <- fromReadFunctionM (applyEdits edits) $ get widget toggleButtonActive;
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
        Refl <- testEquality (info :: Info (WholeEdit Bool)) tedit;
        return $ checkButtonView "";
    };
}
