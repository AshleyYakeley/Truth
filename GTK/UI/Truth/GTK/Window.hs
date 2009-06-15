module UI.Truth.GTK.Window where
{
	import UI.Truth.GTK.View;
	import Graphics.UI.Gtk;
	import Data.Changes;
	import Data.IORef;

	makeWindow :: (WidgetClass w) => IORef Int -> InternalViewFactory w a -> Subscribe a -> IO (Subscribe a);
	makeWindow windowCount ivf sub = do
	{
		(sub',w,close) <- makeView ivf sub;
		window <- windowNew;
		set window [containerChild := w];
		widgetShow w;
		onDestroy window (do
		{
			close;
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
}
