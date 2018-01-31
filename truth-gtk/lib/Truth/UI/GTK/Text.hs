module Truth.UI.GTK.Text
    ( textAreaGetView
    ) where

import Graphics.UI.Gtk
import Shapes
import System.Glib
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful

replaceText :: Index s ~ Int => TextBuffer -> SequenceRun s -> String -> IO ()
replaceText buffer (MkSequenceRun (MkSequencePoint start) (MkSequencePoint len)) text = do
    startIter <- textBufferGetIterAtOffset buffer start
    if len > 0
        then do
            endIter <- textBufferGetIterAtOffset buffer (start + len)
            textBufferDelete buffer startIter endIter
        else return ()
    case text of
        [] -> return ()
        _ -> textBufferInsert buffer startIter text

getSequencePoint :: (Index s ~ Int, MonadIO m) => TextIter -> m (SequencePoint s)
getSequencePoint iter = do
    p <- liftIO $ textIterGetOffset iter
    return $ MkSequencePoint p

getSequenceRun :: (Index s ~ Int, MonadIO m) => TextIter -> TextIter -> m (SequenceRun s)
getSequenceRun iter1 iter2 = do
    p1 <- getSequencePoint iter1
    p2 <- getSequencePoint iter2
    return $ startEndRun p1 p2

textView ::
       forall s. (IsSequence s, Index s ~ Int, Element s ~ Char, GlibString s)
    => UIText (StringEdit s)
    -> GCreateView (StringEdit s)
textView uitext = do
    buffer <- liftIO $ textBufferNew Nothing
    initial <- liftOuter $ viewObjectRead $ mutableReadToSubject
    liftIO $ textBufferSetText buffer initial
    mv <- liftIO $ newMVar ()
    _ <-
        liftOuter $
        liftIOView $ \unlift ->
            on buffer bufferInsertText $ \iter text ->
                ifMVar mv $
                unlift $
                viewObjectPushEdit $ \push -> do
                    p <- getSequencePoint iter
                    push $ pure $ StringReplaceSection (MkSequenceRun p 0) text
    _ <-
        liftOuter $
        liftIOView $ \unlift ->
            on buffer deleteRange $ \iter1 iter2 ->
                ifMVar mv $
                unlift $
                viewObjectPushEdit $ \push -> do
                    run <- getSequenceRun iter1 iter2
                    push $ pure $ StringReplaceSection run mempty
    widget <- liftIO $ textViewNewWithBuffer buffer
    createViewReceiveUpdate $ \_ edit ->
        liftIO $
        ifMVar mv $
        case edit of
            StringReplaceWhole text -> textBufferSetText buffer text
            StringReplaceSection bounds text -> replaceText buffer bounds $ otoList text
    let
        aspect :: Aspect (StringEdit s)
        aspect = do
            (iter1, iter2) <- textBufferGetSelectionBounds buffer
            run <- getSequenceRun iter1 iter2
            -- get selection...
            lens <- stringSectionLens run
            return $ Just ("section", uiLens lens $ MkUISpec uitext)
    createViewAddAspect aspect
    _ <-
        liftOuter $
        liftIOView $ \unlift ->
            on widget focus $ \_ ->
                unlift $ do
                    viewSetSelectedAspect aspect
                    return True
    return $ toWidget widget

textAreaGetView :: GetGView
textAreaGetView =
    MkGetView $ \_ uispec ->
        fmap
            (\case
                 MkStringUIText -> textView MkStringUIText
                 MkTextUIText -> textView MkTextUIText) $
        isUISpec uispec