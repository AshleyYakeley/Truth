module UI.Truth.GTK.Text where
{
	import UI.Truth.GTK.View;
	import Graphics.UI.Gtk;
	import Data.Changes;
--	import Data.FunctorOne;
--	import Data.Result;
--	import Data.IORef;
--	import Control.Applicative;

	replaceText :: TextBuffer -> (Int,Int) -> String -> IO ();
	replaceText buffer (start,len) text = do
	{
		startIter <- textBufferGetIterAtOffset buffer start;
		if len > 0 then do
		{
			endIter <- textBufferGetIterAtOffset buffer (start + len);
			textBufferDelete buffer startIter endIter;
		} else return ();
		case text of
		{
			[] -> return ();
			_ -> textBufferInsert buffer startIter text;
		};
	};

	textIVF :: InternalViewFactory String;
	textIVF initial _push = do
	{
		buffer <- textBufferNew Nothing;
		textBufferSetText buffer initial;
		textView <- textViewNewWithBuffer buffer;
		return (MkInternalView
		{
			ivWidget = textView,
			ivUpdate = \edit -> (putStrLn "edit") >> (case edit of
			{
				ReplaceEdit text -> textBufferSetText buffer text;
				PartEdit (ReplaceSectionEdit bounds text) -> replaceText buffer bounds text;
				PartEdit (ItemEdit i (ReplaceEdit c)) -> replaceText buffer (i,1) [c];
				_ -> return ();
			})
		});
	};
}
