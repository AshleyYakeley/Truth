{-# OPTIONS -fno-warn-orphans #-}
module UI.Truth.GTK.Text (textView) where
{
    import UI.Truth.GTK.GView;
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

    textView :: GView (ListEdit (WholeEdit Char));
    textView initial push = do
    {
        buffer <- textBufferNew Nothing;
        textBufferSetText buffer initial;
        mv <- newMVar ();
        onBufferInsertText buffer (\iter text -> ifMVar mv (do
        {
            i <- textIterGetOffset iter;
            ms <- push (ReplaceSectionEdit (i,0) text);
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
            ms <- push (ReplaceSectionEdit (i1,i2 - i1) "");
            case ms of
            {
                Just _ -> return ();
                _ -> signalStopEmission buffer "delete-range";
            };
        }));
        tv <- textViewNewWithBuffer buffer;
        return (MkViewResult
        {
            vrWidget = MkWidgetStuff (toWidget tv) (do
            {
                (iter1,iter2) <- textBufferGetSelectionBounds buffer;
                o1 <- textIterGetOffset iter1;
                o2 <- textIterGetOffset iter2;
                -- get selection...
                return (Just (MkSelection listSection (o1,o2)));
            }),
            vrUpdate = \edit -> withMVar mv (\_ -> case edit of
            {
                ReplaceListEdit text -> textBufferSetText buffer text;
                ReplaceSectionEdit bounds text -> replaceText buffer bounds text;
                ItemEdit i (MkWholeEdit c) -> replaceText buffer (i,1) [c];
            })
        });
    };
}
