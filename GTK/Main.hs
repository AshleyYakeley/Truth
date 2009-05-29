module Main where
{
	import UI.Truth.GTK;
	import Graphics.UI.Gtk hiding (Object);
	import Data.Changes;
	import Data.Changes.File.Linux;
	import Data.IORef;

	makeWindow :: IORef Int -> String -> Subscribe Bool -> IO (Subscribe Bool);
	makeWindow windowCount name sub = do
	{
		(sub',view) <- makeView (checkButtonIVF name) sub;
		window <- windowNew;
		(\(MkView w _) -> set window [containerChild := w]) view;
		onDestroy window (do
		{
			viewRequestClose view;
			i <- readIORef windowCount;
			writeIORef windowCount (i - 1);
			if i == 1
			 then mainQuit
			 else return ();
		});
		i <- readIORef windowCount;
		writeIORef windowCount (i + 1);		
		widgetShowAll window;
		return sub';
	};

	main :: IO ();
	main = withINotifyB (\_inotify -> do
	{
		initGUI;
		windowCount <- newIORef 0;
		--view <- sourceViewBrowser (linuxFileObject inotify "somefile");
		sub <- makeWindow windowCount "A" (freeObjSubscribe False);
		makeWindow windowCount "B" sub;
		makeWindow windowCount "C" sub;
		mainGUI;
	});	
}
