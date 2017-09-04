module Truth.UI.GTK.CheckButton(checkButtonUIView) where
{
    import Shapes;
    import Graphics.UI.Gtk as Gtk;
    import Truth.Core;
    import Truth.UI.GTK.GView;
    import Truth.UI.GTK.Useful;


    checkButtonUIView :: GetUIView;
    checkButtonUIView = MkGetUIView $ \_ uispec -> fmap (\(MkUICheckbox name) -> MkView $ \(MkObject object) _setSelect -> do
    {
        widget <- checkButtonNew;
        initial <- object $ \muted -> unReadable pureFromReader $ mutableRead muted;
        set widget [buttonLabel := name,toggleButtonActive := initial];
        clickConnection <- onClicked widget $ object $ \muted -> do
        {
            s <- liftIO $ Gtk.get widget toggleButtonActive;
            _ <- mutableEdit muted $ getReplaceEdits s;
            return ();
        };

        let
        {
            vrWidget = toWidget widget;
            vrUpdate :: forall m. IsStateIO m => MutableRead m (WholeReader Bool) -> [WholeEdit Bool] -> m ();
            vrUpdate _ edits = liftIO $ do
            {
                newstate <- fromReadFunctionM (applyEdits edits) $ Gtk.get widget toggleButtonActive;
                withSignalBlocked clickConnection $ set widget [toggleButtonActive := newstate];
                return ();
            };
            vrFirstAspectGetter = return Nothing;
        };
        return MkViewResult{..};
    }) $ isUISpec uispec;
}
