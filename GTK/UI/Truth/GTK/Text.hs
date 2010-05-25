{-# OPTIONS -fno-warn-orphans #-}
module UI.Truth.GTK.Text (textMatchView) where
{
    import UI.Truth.GTK.GView;
    import UI.Truth.GTK.Useful;
    import Graphics.UI.Gtk;
    import Truth.Object;
    import Truth.Edit;
    import Truth.TypeKT;
    import Control.Concurrent.MVar;
    import Data.Witness;

    replaceText :: TextBuffer -> ListRegion -> String -> IO ();
    replaceText buffer (MkListRegion start len) text = do
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
            ms <- push (ReplaceSectionEdit (MkListRegion i 0) text);
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
            ms <- push (ReplaceSectionEdit (MkListRegion i1 (i2 - i1)) "");
            case ms of
            {
                Just _ -> return ();
                _ -> signalStopEmission buffer "delete-range";
            };
        }));
        tv <- textViewNewWithBuffer buffer;
        return (MkViewResult
        {
            vrWidgetStuff = MkViewWidgetStuff (toWidget tv) (do
            {
                (iter1,iter2) <- textBufferGetSelectionBounds buffer;
                o1 <- textIterGetOffset iter1;
                o2 <- textIterGetOffset iter2;
                -- get selection...
                return (Just (MkAspect infoT infoT (listSection (MkListRegion o1 (o2 - o1)))));
            }),
            vrUpdate = \edit -> withMVar mv (\_ -> case edit of
            {
                ReplaceListEdit text -> textBufferSetText buffer text;
                ReplaceSectionEdit bounds text -> replaceText buffer bounds text;
                ItemEdit (MkIndexEdit i (MkWholeEdit c)) -> replaceText buffer (MkListRegion i 1) [c];
            })
        });
    };

    textMatchView :: MatchView;
    textMatchView tedit = do
    {
        MkEqualType <- matchWitnessT tedit (infoT :: InfoT (ListEdit (WholeEdit Char)));
        return textView;
    };
}
