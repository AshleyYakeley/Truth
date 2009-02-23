module UI.Truth.GTK.SourceView where
{
	import Graphics.UI.Gtk hiding (Object);
	import Data.Changes;
	
	data View = forall w. (WidgetClass w) => MkView
	{
		viewWidget :: w,
		--viewSelection :: IO Selection,
		viewRequestClose :: IO Bool
	};

	--type ViewFactory context a = Object context a -> (Selection -> IO ()) -> IO View;
	type ViewFactory context a = Object context a -> IO View;

	sourceViewBrowser :: ViewFactory context a;
	sourceViewBrowser _ = do
	{
		button <- buttonNew;
		set button [ buttonLabel := "Push Me" ];
		onClicked button (putStrLn "Button!");
		return (MkView
		{
			viewWidget = button,
			viewRequestClose = return True
		});
	};
}
