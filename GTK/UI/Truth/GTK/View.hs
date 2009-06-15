module UI.Truth.GTK.View where
{
	import Graphics.UI.Gtk;
	import System.Glib.Types;
	import Data.Changes;
	import Control.Exception;
	import Control.Concurrent.MVar;

	data ViewResult w a = MkViewResult
	{
		vrWidget :: w,
		vrUpdate :: Edit a -> IO ()
	};

	type View w a = a -> Push a -> IO (ViewResult w a);

	makeView :: View w a -> Subscribe a -> IO (Subscribe a,w,IO ());
	makeView view subscribe = do
	{
		(vr,sub) <- subscribe view vrUpdate;
		return (subCopy sub,vrWidget vr,subClose sub);
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
