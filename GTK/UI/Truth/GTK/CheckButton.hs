{-# OPTIONS -fno-warn-orphans #-}
module UI.Truth.GTK.CheckButton where
{
    import UI.Truth.GTK.GView;
    import UI.Truth.GTK.Useful;
    import Graphics.UI.Gtk;
    import Data.Changes;
    import Data.ConstFunction;
    
    checkButtonView :: String -> GView (WholeEdit Bool);
    checkButtonView name initial push = do
    {
        widget <- checkButtonNew;
        set widget [buttonLabel := name,toggleButtonActive := initial];
        clickConnection <- onClicked widget (do
        {
            s <- get widget toggleButtonActive;
            push (replaceEdit s);
            return ();
        });
        return (MkViewResult
        {
            vrWidgetStuff = MkViewWidgetStuff (toWidget widget) (return Nothing),
            vrUpdate = \edit -> do
            {
                newstate <- applyConstFunctionA (applyEdit edit) (get widget toggleButtonActive);
                withSignalBlocked clickConnection
                    (set widget [toggleButtonActive := newstate]);
            }
        });
    };
}
