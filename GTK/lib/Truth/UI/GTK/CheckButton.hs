{-# OPTIONS -fno-warn-orphans #-}
module Truth.UI.GTK.CheckButton(checkButtonTypeKnowledge) where
{
    import Shapes;
    import Graphics.UI.Gtk as Gtk;
    import Data.Reity;
    import Truth.Core;
    import Truth.UI.GTK.GView;
    import Truth.UI.GTK.Useful;


    checkButtonView :: String -> GView (WholeEdit Bool);
    checkButtonView name = MkView $ \(MkObject object) _setSelect -> do
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
    };

    -- orphan
    instance DependentHasView Widget (WholeEdit Bool);
    -- orphan
    instance HasView Widget (WholeEdit Bool) where
    {
        theView = checkButtonView "";
    };

    checkButtonTypeKnowledge :: TypeKnowledge;
    checkButtonTypeKnowledge = namedKnowledge "check button" $(generateTypeKnowledge [d|
        instance DependentHasView Widget (WholeEdit Bool);
    |]);
}
