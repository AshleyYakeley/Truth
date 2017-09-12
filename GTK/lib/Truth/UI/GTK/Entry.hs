module Truth.UI.GTK.Entry(textEntryUIView) where
{
    import Shapes;
    import Graphics.UI.Gtk as Gtk;
    import Truth.Core;
    import Truth.UI.GTK.GView;
    import Truth.UI.GTK.Useful;


    textEntryUIView :: GetUIView;
    textEntryUIView = MkGetUIView $ \_ uispec -> fmap (\MkUITextEntry -> MkView $ \(MkObject object) _setSelect -> do
    {
        widget <- entryNew;
        initial <- object $ \muted -> unReadable fromReader $ mutableRead muted;
        set widget [entryText := initial];
        clickConnection <- on widget editableChanged $ object $ \muted -> do
        {
            s <- liftIO $ Gtk.get widget entryText;
            edits <- getReplaceEditsM s;
            _ <- mutableEdit muted edits;
            return ();
        };

        let
        {
            vrWidget = toWidget widget;
            vrUpdate :: forall m. IsStateIO m => MutableRead m (WholeReader String) -> [WholeEdit String] -> m ();
            vrUpdate _ edits = liftIO $ do
            {
                newstate <- fromReadFunctionM (applyEdits edits) $ Gtk.get widget entryText;
                withSignalBlocked clickConnection $ set widget [entryText := newstate];
                return ();
            };
            vrFirstAspectGetter = return Nothing;
        };
        return MkViewResult{..};
    }) $ isUISpec uispec;
}
