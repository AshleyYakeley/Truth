module UI.Truth.GTK.View where
{
	import Graphics.UI.Gtk;
	import System.Glib.Types;
	import Data.Changes;
	import Control.Exception;
	import Control.Concurrent.MVar;

	data InternalView w a = MkInternalView
	{
		ivWidget :: w,
		ivUpdate :: Edit a -> IO ()
	};

	type InternalViewFactory w a = a -> Push a -> IO (InternalView w a);

	makeView :: InternalViewFactory w a -> Subscribe a -> IO (Subscribe a,w,IO ());
	makeView ivf subscribe = do
	{
		(view,sub) <- subscribe ivf ivUpdate;
		return (subCopy sub,ivWidget view,subClose sub);
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
