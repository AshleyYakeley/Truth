module UI.Truth.GTK.Text(textView) where
{
	import UI.Truth.GTK.Useful;
	import Graphics.UI.Gtk;
	import Data.Changes;
	import Control.Concurrent.MVar;

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

	textView :: View TextView String;
	textView initial push = do
	{
		buffer <- textBufferNew Nothing;
		textBufferSetText buffer initial;
		mv <- newMVar ();
		onBufferInsertText buffer (\iter text -> ifMVar mv (do
		{
			i <- textIterGetOffset iter;
			ms <- push (PartEdit (ReplaceSectionEdit (i,0) text));
			case ms of
			{
				Just _ -> return ();
				_ -> signalStopEmission buffer "insert-text";
			};
		}));
		onDeleteRange buffer (\iter1 iter2 -> ifMVar mv (do
		{
			i1 <- textIterGetOffset iter1;
			i2 <- textIterGetOffset iter2;
			ms <- push (PartEdit (ReplaceSectionEdit (i1,i2 - i1) ""));
			case ms of
			{
				Just _ -> return ();
				_ -> signalStopEmission buffer "delete-range";
			};
		}));
		tv <- textViewNewWithBuffer buffer;
		return (MkViewResult
		{
			vrWidget = tv,
			vrUpdate = \edit -> withMVar mv (\_ -> case edit of
			{
				ReplaceEdit text -> textBufferSetText buffer text;
				PartEdit (ReplaceSectionEdit bounds text) -> replaceText buffer bounds text;
				PartEdit (ItemEdit i (ReplaceEdit c)) -> replaceText buffer (i,1) [c];
				_ -> return ();
			})
		});
	};
}
