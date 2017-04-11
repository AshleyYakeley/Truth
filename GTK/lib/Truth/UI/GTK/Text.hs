{-# OPTIONS -fno-warn-orphans #-}
module Truth.UI.GTK.Text (textMatchView) where
{
    import Data.Foldable;
    import Control.Concurrent.MVar;
    import Control.Monad.IO.Class;
    import Graphics.UI.Gtk;
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
    textView = MkView $ \(MkLockAPI lapi) setSelect -> do
    {
        buffer <- textBufferNew Nothing;
        initial <- lapi $ \() api -> unReadable fromReader $ apiRead api;
        textBufferSetText buffer initial;
        mv <- newMVar ();

        _ <- onBufferInsertText buffer $ \iter text -> lapi $ \() api -> ifMVar mv $ do
        {
            p <- getSequencePoint iter;
            ms <- apiEdit api $ pure $ StringReplaceSection (MkSequenceRun p 0) text;
            case ms of
            {
                Just _ -> return ();
                _ -> liftIO $ signalStopEmission buffer "insert-text";
            };
        };

        _ <- onDeleteRange buffer $ \iter1 iter2 -> lapi $ \() api -> ifMVar mv $ do
        {
            run <- getSequenceRun iter1 iter2;
            ms <- apiEdit api $ pure $ StringReplaceSection run "";
            case ms of
            {
                Just _ -> return ();
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

            vrUpdate :: () -> [StringEdit String] -> IO ();
            -- this withMVar prevents the signal handlers from re-sending edits
            vrUpdate () edits = withMVar mv $ \_ -> traverse_ update edits;

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
    textMatchView tedit = do
    {
        Refl <- testEquality (info :: Info (StringEdit String)) tedit;
        return textView;
    };
}
