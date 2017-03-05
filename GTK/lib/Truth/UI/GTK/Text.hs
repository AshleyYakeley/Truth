{-# OPTIONS -fno-warn-orphans #-}
module Truth.UI.GTK.Text (textMatchView) where
{
    import Graphics.UI.Gtk;
    import Data.Reity;
    import Truth.Edit;
    import Truth.Object;
    import Truth.UI.GTK.GView;
    import Truth.UI.GTK.Useful;

    import Control.Concurrent.MVar;
    import Data.Witness;

    replaceText :: TextBuffer -> SequenceRun String -> String -> IO ();
    replaceText buffer (MkSequenceRun (MkSequencePoint start) (MkSequencePoint len)) text = do
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

    textView :: GView (StringEdit String);
    textView = MkView $ \lapi -> do
    {
        buffer <- textBufferNew Nothing;
        initial <- lapi $ \() api -> unReadable fromReader $ apiRead api;
        textBufferSetText buffer initial;
        mv <- newMVar ();

        _ <- onBufferInsertText buffer $ \iter text -> lapi $ \() api -> ifMVar mv $ do
        {
            i <- liftIO $ textIterGetOffset iter;
            ms <- apiEdit api $ StringReplaceSection (MkSequenceRun (MkSequencePoint i) 0) text;
            case ms of
            {
                Just _ -> return ();
                _ -> liftIO $ signalStopEmission buffer "insert-text";
            };
        };

        _ <- onDeleteRange buffer $ \iter1 iter2 -> lapi $ \() api -> ifMVar mv $ do
        {
            (MkSequencePoint -> i1) <- liftIO $ textIterGetOffset iter1;
            (MkSequencePoint -> i2) <- liftIO $ textIterGetOffset iter2;
            ms <- apiEdit api $ StringReplaceSection (startEndRun i1 i2) "";
            case ms of
            {
                Just _ -> return ();
                _ -> liftIO $ signalStopEmission buffer "delete-range";
            };
        };

        widget <- textViewNewWithBuffer buffer;
        let
        {
            vrWidgetStuff = MkViewWidgetStuff (toWidget widget) $ do
            {
                (iter1,iter2) <- textBufferGetSelectionBounds buffer;
                (MkSequencePoint -> o1) <- textIterGetOffset iter1;
                (MkSequencePoint -> o2) <- textIterGetOffset iter2;
                -- get selection...
                return (Just (MkAspect info info (stringSectionLens (MkSequenceRun o1 (o2 - o1)))));
            };

            vrUpdate () edit = withMVar mv $ \_ -> case edit of
            {
                StringReplaceWhole text -> textBufferSetText buffer text;
                StringReplaceSection bounds text -> replaceText buffer bounds text;
            };
        };
        return (MkViewResult{..},());
    };

    textMatchView :: MatchView;
    textMatchView tedit = do
    {
        Refl <- testEquality (info :: Info (StringEdit String)) tedit;
        return textView;
    };
}
