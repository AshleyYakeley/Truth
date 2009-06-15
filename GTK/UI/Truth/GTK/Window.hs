module UI.Truth.GTK.Window where
{
	import UI.Truth.GTK.View;
	import Graphics.UI.Gtk;
	import Data.Changes;
	import Data.IORef;

	makeWindow :: (WidgetClass w) => IO () -> View w a -> Subscribe a -> IO (Subscribe a);
	makeWindow tellclose view sub = do
	{
		(sub',w,close) <- makeView view sub;
		window <- windowNew;
		set window [containerChild := w];
		widgetShow w;
		onDestroy window (do
		{
			close;
			tellclose;
		});
		widgetShow window;
		return sub';
	};

	makeWindowCountRef :: (WidgetClass w) => IORef Int -> View w a -> Subscribe a -> IO (Subscribe a);
	makeWindowCountRef windowCount view sub = do
	{
		sub' <- makeWindow (do
		{
			i <- readIORef windowCount;
			writeIORef windowCount (i - 1);
			if i == 1
			 then mainQuit
			 else return ();
		}) view sub;
		i <- readIORef windowCount;
		writeIORef windowCount (i + 1);
		return sub';
	};
}
