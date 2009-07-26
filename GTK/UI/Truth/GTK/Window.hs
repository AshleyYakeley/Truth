module UI.Truth.GTK.Window where
{
	import UI.Truth.GTK.GView;
	import Graphics.UI.Gtk;
	import Data.Changes;
	import Data.IORef;

	makeWindow :: (HasGView a) => IO () -> Subscribe a -> IO (Subscribe a);
	makeWindow tellclose sub = do
	{
		(sub',w,close) <- subscribeView gView sub;
		window <- windowNew;
		set window [containerChild := wsWidget w];
		widgetShow (wsWidget w);
		onDestroy window (do
		{
			close;
			tellclose;
		});
		widgetShow window;
		return sub';
	};

	makeWindowCountRef :: (HasGView a) => IORef Int -> Subscribe a -> IO (Subscribe a);
	makeWindowCountRef windowCount sub = do
	{
		sub' <- makeWindow (do
		{
			i <- readIORef windowCount;
			writeIORef windowCount (i - 1);
			if i == 1
			 then mainQuit
			 else return ();
		}) sub;
		i <- readIORef windowCount;
		writeIORef windowCount (i + 1);
		return sub';
	};
}
