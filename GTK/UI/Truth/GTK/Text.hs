module UI.Truth.GTK.Text where
{
	import UI.Truth.GTK.View;
	import Graphics.UI.Gtk;
--	import Data.Changes;
--	import Data.FunctorOne;
--	import Data.Result;
--	import Data.IORef;
--	import Control.Applicative;

	textIVF :: InternalViewFactory String;
	textIVF initial _push = do
	{
		buffer <- textBufferNew Nothing;
		textBufferSetText buffer initial;
		textView <- textViewNewWithBuffer buffer;
		return (MkInternalView
		{
			ivWidget = textView,
			ivUpdate = \_edit -> do
			{
				return ();
			}
		});
	};
}
