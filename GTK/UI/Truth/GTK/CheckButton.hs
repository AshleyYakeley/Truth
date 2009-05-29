module UI.Truth.GTK.CheckButton where
{
	import UI.Truth.GTK.View;
	import Graphics.UI.Gtk;
	import Data.Changes;
	import Data.ConstFunction;
	
	checkButtonIVF :: String -> InternalViewFactory Bool;
	checkButtonIVF name initial push = do
	{
		widget <- checkButtonNew;
		set widget [buttonLabel := name,toggleButtonActive := initial];
		clickConnection <- onClicked widget (do
		{
			_ <- push (do
			{
				s <- get widget toggleButtonActive;
				return (Just (ReplaceEdit s));
			});
			return ();
		});
		return (MkInternalView
		{
			ivWidget = widget,
			ivUpdate = \edit -> do
			{
				newstate <- applyConstFunctionA (applyEdit edit) (get widget toggleButtonActive);
				withSignalBlocked clickConnection
					(set widget [toggleButtonActive := newstate]);
			}
		});
	};
}
