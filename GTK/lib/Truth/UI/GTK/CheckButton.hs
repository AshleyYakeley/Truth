{-# OPTIONS -fno-warn-orphans #-}
module Truth.UI.GTK.CheckButton(checkButtonTypeKnowledge) where
{
    import Data.Empty;
    import Graphics.UI.Gtk;
    import Control.Monad.IsStateIO;
    import Data.Reity;
    import Truth.Core;
    import Truth.UI.GTK.GView;
    import Truth.UI.GTK.Useful;


    checkButtonView :: String -> GView (WholeEdit Bool);
    checkButtonView name = MkView $ \(MkObject object) _setSelect -> do
    {
        widget <- checkButtonNew;
        initial <- object $ \muted -> unReadable fromReader $ mutableRead muted;
        set widget [buttonLabel := name,toggleButtonActive := initial];
        clickConnection <- onClicked widget $ object $ \muted -> do
        {
            s <- liftIO $ get widget toggleButtonActive;
            _ <- mutableEdit muted $ getReplaceEdits s;
            return ();
        };

        let
        {
            vrWidget = toWidget widget;
            vrUpdate :: forall m. IsStateIO m => MutableRead m (WholeReader Bool) -> [WholeEdit Bool] -> m ();
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

    -- orphan
    instance DependentHasGView (WholeEdit Bool);
    -- orphan
    instance HasGView (WholeEdit Bool) where
    {
        gview = checkButtonView "";
    };

    checkButtonTypeKnowledge :: TypeKnowledge;
    checkButtonTypeKnowledge = namedKnowledge "check button" $(declInfo [d|
        instance DependentHasGView (WholeEdit Bool);
    |]);
}
