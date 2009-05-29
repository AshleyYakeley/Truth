module Main where
{
	import UI.Truth.GTK;
	import Graphics.UI.Gtk hiding (Object);
	import Data.Changes;
	import Data.Changes.File.Linux;

	makeWindow :: String -> Subscribe Bool -> IO (Subscribe Bool);
	makeWindow name sub = do
	{
		(sub',view) <- makeView (checkButtonIVF name) sub;
		window <- windowNew;
		(\(MkView w _) -> set window [containerChild := w]) view;
		onDestroy window mainQuit;
		widgetShowAll window;
		return sub';
	};

	main :: IO ();
	main = withINotifyB (\_inotify -> do
	{
		initGUI;
		--view <- sourceViewBrowser (linuxFileObject inotify "somefile");
		sub <- makeWindow "A" (freeObjSubscribe False);
		makeWindow "B" sub;
		mainGUI;
	});	
}
