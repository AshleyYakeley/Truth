{-# OPTIONS -fno-warn-orphans #-}
module Truth.UI.GTK.Text (textMatchView) where
{
    import Data.Foldable;
    import Control.Concurrent.MVar;
    import Control.Monad.IO.Class;
    import Graphics.UI.Gtk;
    import Control.Monad.IOInvert;
    import Data.Witness;
    import Data.Reity;
    import Truth.Core;
    import Truth.UI.GTK.GView;
    import Truth.UI.GTK.Useful;


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

    getSequencePoint :: MonadIO m => TextIter -> m (SequencePoint String);
    getSequencePoint iter = do
    {
        p <- liftIO $ textIterGetOffset iter;
        return $ MkSequencePoint p;
    };

    getSequenceRun :: MonadIO m => TextIter -> TextIter -> m (SequenceRun String);
    getSequenceRun iter1 iter2 = do
    {
        p1 <- getSequencePoint iter1;
        p2 <- getSequencePoint iter2;
        return $ startEndRun p1 p2;
    };

    textView :: GView (StringEdit String);
    textView = MkView $ \(MkObject object) setSelect -> do
    {
        buffer <- textBufferNew Nothing;
        initial <- object $ \_ muted -> unReadable fromReader $ mutableRead muted;
        textBufferSetText buffer initial;
        mv <- newMVar ();

        _ <- onBufferInsertText buffer $ \iter text -> object $ \() muted -> withMVar' mv $ \_ -> do
        {
            p <- getSequencePoint iter;
            maction <- mutableEdit muted $ pure $ StringReplaceSection (MkSequenceRun p 0) text;
            case maction of
            {
                Just action -> action;
                _ -> liftIO $ signalStopEmission buffer "insert-text";
            };
        };

        _ <- onDeleteRange buffer $ \iter1 iter2 -> object $ \() muted -> withMVar' mv $ \_ -> do
        {
            run <- getSequenceRun iter1 iter2;
            maction <- mutableEdit muted $ pure $ StringReplaceSection run "";
            case maction of
            {
                Just action -> action;
                _ -> liftIO $ signalStopEmission buffer "delete-range";
            };
        };

        widget <- textViewNewWithBuffer buffer;

        _ <- onFocus widget $ \_ -> do
        {
            setSelect ();
            return True;
        };

        let
        {
            vrWidget :: Widget;
            vrWidget = toWidget widget;

            vrFirstUpdateState :: ();
            vrFirstUpdateState = ();

            update :: StringEdit String -> IO ();
            update (StringReplaceWhole text) = textBufferSetText buffer text;
            update (StringReplaceSection bounds text) = replaceText buffer bounds text;

            vrUpdate :: forall m. MonadIOInvert m => MutableRead m (StringRead String) -> () -> [StringEdit String] -> m ();
            -- this withMVar prevents the signal handlers from re-sending edits
            vrUpdate _ () edits = liftIO $ ifMVar mv $ traverse_ update edits;

            vrFirstSelState :: Maybe ();
            vrFirstSelState = Just ();

            vrGetSelection :: () -> IO (Maybe (Aspect (StringEdit String)));
            vrGetSelection _ = do
            {
                (iter1,iter2) <- textBufferGetSelectionBounds buffer;
                run <- getSequenceRun iter1 iter2;
                -- get selection...
                return $ Just $ MkAspect info info $ MkCloseFloat $ stringSectionLens run;
            };
        };
        return MkViewResult{..};
    };

    textMatchView :: MatchView;
    textMatchView = MkMatchView $ \tedit -> do
    {
        Refl <- testEquality (info :: Info (StringEdit String)) tedit;
        return textView;
    };
}
