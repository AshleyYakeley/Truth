module UI.Truth.GTK.View where
{
	import Graphics.UI.Gtk;
	import Data.Changes;
	import Control.Exception;
	import System.Glib.Types;
	
	data View = forall w. (WidgetClass w) => MkView
	{
		viewWidget :: w,
		--viewSelection :: IO Selection,
		viewRequestClose :: IO Bool
	};

	--type ViewFactory a = Subscribe a -> (Selection -> IO ()) -> IO View;
	type ViewFactory a = Subscribe a -> IO (Subscribe a,View);

	data InternalView a = forall w. (WidgetClass w) => MkInternalView
	{
		ivWidget :: w,
		ivUpdate :: Edit a -> IO ()
	};

	type InternalViewFactory a = a -> Push a -> IO (InternalView a);

	makeView :: InternalViewFactory a -> ViewFactory a;
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
}
