{-# OPTIONS -fno-warn-orphans #-}
module UI.Truth.GTK.CheckButton where
{
    import UI.Truth.GTK.GView;
    import UI.Truth.GTK.Useful;
    import Graphics.UI.Gtk;
    import Truth.Object;
    import Truth.Edit;
    import Truth.TypeKT;
    import Data.ConstFunction;
    import Data.Witness;

    checkButtonView :: String -> GView (WholeEdit Bool);
    checkButtonView name initial push = do
    {
        widget <- checkButtonNew;
        set widget [buttonLabel := name,toggleButtonActive := initial];
        clickConnection <- onClicked widget (do
        {
            s <- get widget toggleButtonActive;
            _ <- push (replaceEdit s);
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

    checkButtonMatchView :: MatchView;
    checkButtonMatchView tedit = do
    {
        MkEqualType <- matchProp $(type1[t|EqualType (Type_T (WholeEdit Bool))|]) tedit;
        return (checkButtonView "");
    };
}
