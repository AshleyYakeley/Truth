module Main where
{
	import UI.Truth.GTK.SourceView;
	import Graphics.UI.Gtk hiding (Object);
	--import Data.Changes;
	import Data.Changes.File.Linux;

	main :: IO ();
	main = withINotifyB (\inotify -> do
	{
		initGUI;
		view <- sourceViewBrowser (linuxFileObject inotify "somefile");
		window <- windowNew;
		set window [containerChild := view];
		onDestroy window mainQuit;
		widgetShowAll window;
		mainGUI;
	});	
}
