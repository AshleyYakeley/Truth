module Truth.UI.GTK.CheckButton(checkButtonGetView) where
{
    import Shapes;
    import Graphics.UI.Gtk as Gtk;
    import Truth.Core;
    import Truth.UI.GTK.GView;
    import Truth.UI.GTK.Useful;


    checkButtonGetView :: GetGView;
    checkButtonGetView = MkGetView $ \_ uispec -> fmap (\(MkUICheckbox name) -> do
    {
        widget <- liftIO checkButtonNew;
        initial <- viewMutableRead $ unReadable subjectFromReader;
        liftIO $ set widget [buttonLabel := name,toggleButtonActive := initial];

        changedSignal <- viewOn widget buttonActivated $ viewMutableEdit $ \muted -> do
        {
            st <- liftIO $ Gtk.get widget toggleButtonActive;
            maction <- mutableEdit muted [MkWholeEdit st];
            case maction of
            {
                Just action -> action;
                Nothing -> return ();
            };
        };

        viewReceiveUpdate $ \_ (MkWholeEdit st) -> liftIO $ withSignalBlocked changedSignal $ set widget [toggleButtonActive := st];

        return $ toWidget widget;
    }) $ isUISpec uispec;
}
