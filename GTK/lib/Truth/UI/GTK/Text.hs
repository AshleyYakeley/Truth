{-# OPTIONS -fno-warn-orphans #-}
module Truth.UI.GTK.Text (textTypeKnowledge) where
{
    import Prelude;
    import Data.Foldable;
    import Data.Text;
    import Data.Sequences;
    import Data.MonoTraversable;
    import Control.Concurrent.MVar;
    import Control.Monad.IO.Class;
    import System.Glib;
    import Graphics.UI.Gtk;
    import Control.Monad.IsStateIO;
    import Data.Reity;
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

    textView :: forall s. (IsSequence s,Index s ~ Int,Element s ~ Char,GlibString s,HasTypeInfo s) => GView (StringEdit s);
    textView = MkView $ \(MkObject object) setSelect -> do
    {
        buffer <- textBufferNew Nothing;
        initial <- object $ \muted -> unReadable pureFromReader $ mutableRead muted;
        textBufferSetText buffer initial;
        mv <- newMVar ();

        _ <- onBufferInsertText buffer $ \iter text -> ifMVar mv $ object $ \muted -> do
        {
            p <- getSequencePoint iter;
            maction <- mutableEdit muted $ pure $ StringReplaceSection (MkSequenceRun p 0) text;
            case maction of
            {
                Just action -> action;
                _ -> liftIO $ signalStopEmission buffer "insert-text";
            };
        };

        _ <- onDeleteRange buffer $ \iter1 iter2 -> ifMVar mv $ object $ \muted -> do
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

            vrFirstAspectGetter :: AspectGetter (StringEdit s);
            vrFirstAspectGetter = do
            {
                (iter1,iter2) <- textBufferGetSelectionBounds buffer;
                run <- getSequenceRun iter1 iter2;
                -- get selection...
                return $ Just $ return $ MkAspect typeInfo $ MkCloseState $ pureToEditLens $ stringSectionLens run;
            };
        };

        _ <- onFocus widget $ \_ -> do
        {
            setSelect vrFirstAspectGetter;
            return True;
        };
        return MkViewResult{..};
    };

    -- orphan
    instance DependentHasView Widget (StringEdit String);
    -- orphan
    instance HasView Widget (StringEdit String) where
    {
        theView = textView;
    };
    -- orphan
    instance DependentHasView Widget (StringEdit Text);
    -- orphan
    instance HasView Widget (StringEdit Text) where
    {
        theView = textView;
    };

    textTypeKnowledge :: TypeKnowledge;
    textTypeKnowledge = namedKnowledge "text" $(generateTypeKnowledge [d|
        instance DependentHasView Widget (StringEdit String);
        instance DependentHasView Widget (StringEdit Text);
    |]);
}
