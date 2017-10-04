module Truth.UI.GTK.Text (textUIView) where
{
    import Shapes;
    import System.Glib;
    import Graphics.UI.Gtk;
    import Truth.Core;
    import Truth.UI.GTK.GView;
    import Truth.UI.GTK.Useful;


    replaceText :: Index s ~ Int => TextBuffer -> SequenceRun s -> String -> IO ();
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

    getSequencePoint :: (Index s ~ Int,MonadIO m) => TextIter -> m (SequencePoint s);
    getSequencePoint iter = do
    {
        p <- liftIO $ textIterGetOffset iter;
        return $ MkSequencePoint p;
    };

    getSequenceRun :: (Index s ~ Int,MonadIO m) => TextIter -> TextIter -> m (SequenceRun s);
    getSequenceRun iter1 iter2 = do
    {
        p1 <- getSequencePoint iter1;
        p2 <- getSequencePoint iter2;
        return $ startEndRun p1 p2;
    };

    textView :: forall s. (IsSequence s,Index s ~ Int,Element s ~ Char,GlibString s) => UIText (StringEdit s) -> GView (StringEdit s);
    textView uitext = MkView $ \(MkObject object) setSelect -> do
    {
        buffer <- textBufferNew Nothing;
        initial <- object $ \muted -> unReadable subjectFromReader $ mutableRead muted;
        textBufferSetText buffer initial;
        mv <- newMVar ();

        _ <- on buffer bufferInsertText $ \iter text -> ifMVar mv $ object $ \muted -> do
        {
            p <- getSequencePoint iter;
            maction <- mutableEdit muted $ pure $ StringReplaceSection (MkSequenceRun p 0) text;
            case maction of
            {
                Just action -> action;
                _ -> liftIO $ signalStopEmission buffer "insert-text";
            };
        };

        _ <- on buffer deleteRange $ \iter1 iter2 -> ifMVar mv $ object $ \muted -> do
        {
            run <- getSequenceRun iter1 iter2;
            maction <- mutableEdit muted $ pure $ StringReplaceSection run mempty;
            case maction of
            {
                Just action -> action;
                _ -> liftIO $ signalStopEmission buffer "delete-range";
            };
        };

        widget <- textViewNewWithBuffer buffer;

        let
        {
            vrWidget :: Widget;
            vrWidget = toWidget widget;

            update :: StringEdit s -> IO ();
            update (StringReplaceWhole text) = textBufferSetText buffer text;
            update (StringReplaceSection bounds text) = replaceText buffer bounds $ otoList text;

            vrUpdate :: forall m. IsStateIO m => MutableRead m (StringRead s) -> [StringEdit s] -> m ();
            -- this withMVar prevents the signal handlers from re-sending edits
            vrUpdate _ edits = liftIO $ ifMVar mv $ traverse_ update edits;

            vrFirstAspectGetter :: Aspect (StringEdit s);
            vrFirstAspectGetter = do
            {
                (iter1,iter2) <- textBufferGetSelectionBounds buffer;
                run <- getSequenceRun iter1 iter2;
                -- get selection...
                lens <- stringSectionLens run;
                return $ Just ("section",MkUISpec $ MkUILens (MkCloseState lens) $ MkUISpec uitext);
            };
        };

        _ <- on widget focus $ \_ -> do
        {
            setSelect vrFirstAspectGetter;
            return True;
        };
        return MkViewResult{..};
    };

    textUIView :: GetGView;
    textUIView = MkGetView $ \_ uispec -> fmap (\case
    {
        MkStringUIText -> textView MkStringUIText;
        MkTextUIText -> textView MkTextUIText;
    }) $ isUISpec uispec;
}
