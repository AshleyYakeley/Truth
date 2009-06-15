module UI.Truth.GTK.View where
{
	import Graphics.UI.Gtk;
	import System.Glib.Types;
	import Data.Changes;
	import Control.Exception;
	import Control.Concurrent.MVar;
	
	data View w = MkView
	{
		viewWidget :: w,
		--viewSelection :: IO Selection,
		viewRequestClose :: IO Bool
	};

	--type ViewFactory a = Subscribe a -> (Selection -> IO ()) -> IO View;
	type ViewFactory w a = Subscribe a -> IO (Subscribe a,View w);

	data InternalView w a = MkInternalView
	{
		ivWidget :: w,
		ivUpdate :: Edit a -> IO ()
	};

	type InternalViewFactory w a = a -> Push a -> IO (InternalView w a);

	makeView :: InternalViewFactory w a -> ViewFactory w a;
	makeView ivf subscribe = do
	{
		(view,sub) <- subscribe ivf ivUpdate;
		case view of
		{
			(MkInternalView widget _) -> do
			{
				return (subCopy sub,MkView
				{
					viewWidget = widget,
					viewRequestClose = do
					{
						subClose sub;
						return True;
					}
				});
			};
		};
	};
	
	withSignalBlocked :: (GObjectClass obj) => ConnectId obj -> IO a -> IO a;
	withSignalBlocked conn = bracket_ (signalBlock conn) (signalUnblock conn);

	tryReadMVar :: MVar a -> IO (Maybe a);
	tryReadMVar mv = do
	{
		ma <- tryTakeMVar mv;
		case ma of
		{
			Just a -> putMVar mv a;
			_ -> return ();
		};
		return ma;
	};

	ifMVar :: MVar () -> IO a -> IO ();
	ifMVar mv f = do
	{
		ma <- tryReadMVar mv;
		case ma of
		{
			Just _ -> f >> return ();
			_ -> return ();
		};
	};
}
