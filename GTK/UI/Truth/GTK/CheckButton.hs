{-# OPTIONS -fno-warn-orphans #-}
module UI.Truth.GTK.CheckButton() where
{
	import UI.Truth.GTK.GView;
	import UI.Truth.GTK.Useful;
	import Graphics.UI.Gtk;
	import Data.Changes;
	import Data.ConstFunction;
	
	checkButtonView :: String -> GView Bool;
	checkButtonView name initial push = do
	{
		widget <- checkButtonNew;
		set widget [buttonLabel := name,toggleButtonActive := initial];
		clickConnection <- onClicked widget (do
		{
			s <- get widget toggleButtonActive;
			push (ReplaceEdit s);
			return ();
		});
		return (MkViewResult
		{
			vrWidget = toWidget widget,
			vrUpdate = \edit -> do
			{
				newstate <- applyConstFunctionA (applyEdit edit) (get widget toggleButtonActive);
				withSignalBlocked clickConnection
					(set widget [toggleButtonActive := newstate]);
			}
		});
	};

	instance HasNamedGView Bool where
	{
		gNamedView = checkButtonView;
	};
}
