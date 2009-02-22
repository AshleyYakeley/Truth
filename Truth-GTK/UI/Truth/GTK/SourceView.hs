module UI.Truth.GTK.SourceView where
{
	import Graphics.UI.Gtk hiding (Object);
	import Data.Changes;

	sourceViewBrowser :: Object context a -> IO Button;
	sourceViewBrowser _ = do
	{
		button <- buttonNew;
		set button [ buttonLabel := "Push Me" ];
		onClicked button (putStrLn "Button!");
		return button;
	};
}
