module Main where
{
	import UI.Truth.GTK;
	import Graphics.UI.Gtk;
	import Data.Changes;
	import Data.Changes.File.Linux;
	import Data.IORef;

	makeWindow :: IORef Int -> InternalViewFactory a -> Subscribe a -> IO (Subscribe a);
	makeWindow windowCount ivf sub = do
	{
		(sub',view) <- makeView ivf sub;
		window <- windowNew;
		(\(MkView w _) -> do
		{
			set window [containerChild := w];
			widgetShow w;
		}) view;
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
		widgetShow window;
		return sub';
	};

	initial :: Maybe Bool;
	initial = Just True;

	main :: IO ();
	main = withINotifyB (\_inotify -> do
	{
		initGUI;
		windowCount <- newIORef 0;
		--view <- sourceViewBrowser (linuxFileObject inotify "somefile");
		sub <- makeWindow windowCount (maybeIVF False (checkButtonIVF "AAAAAAAAAAAAAAAAAAAAAAAAAA")) (freeObjSubscribe initial);
		makeWindow windowCount (maybeIVF False (checkButtonIVF "BBBBBBBBBBBBBBBBBBBBBBBBBBBBB")) sub;
		makeWindow windowCount (maybeIVF True (checkButtonIVF "CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC")) sub;
		mainGUI;
	});
}
