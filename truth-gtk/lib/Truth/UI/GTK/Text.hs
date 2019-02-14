module Truth.UI.GTK.Text
    ( textAreaGetView
    ) where

import GI.Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful

replaceText :: Index s ~ Int => TextBuffer -> SequenceRun s -> Text -> IO ()
replaceText buffer (MkSequenceRun (MkSequencePoint start) (MkSequencePoint len)) text = do
    startIter <- #getIterAtOffset buffer (fromIntegral start)
    if len > 0
        then do
            endIter <- #getIterAtOffset buffer (fromIntegral $ start + len)
            #delete buffer startIter endIter
        else return ()
    if onull text
        then return ()
        else #insert buffer startIter text (-1)

getSequencePoint :: (Index s ~ Int, MonadIO m) => TextIter -> m (SequencePoint s)
getSequencePoint iter = do
    p <- #getOffset iter
    return $ MkSequencePoint $ fromIntegral p

getSequenceRun :: (Index s ~ Int, MonadIO m) => TextIter -> TextIter -> m (SequenceRun s)
getSequenceRun iter1 iter2 = do
    p1 <- getSequencePoint iter1
    p2 <- getSequencePoint iter2
    return $ startEndRun p1 p2

textView :: GCreateView (EditLens (StringEdit Text) (StringEdit Text)) (StringEdit Text)
textView = do
    buffer <- new TextBuffer []
    initial <- cvLiftView $ viewObjectRead $ \_ -> mutableReadToSubject
    #setText buffer initial (-1)
    mv <- liftIO $ newMVar ()
    _ <-
        cvLiftView $
        liftIOView $ \unlift ->
            on buffer #insertText $ \iter text _ ->
                ifMVar mv $
                unlift $
                viewObjectPushEdit $ \_ push -> do
                    p <- getSequencePoint iter
                    push $ pure $ StringReplaceSection (MkSequenceRun p 0) text
    _ <-
        cvLiftView $
        liftIOView $ \unlift ->
            on buffer #deleteRange $ \iter1 iter2 ->
                ifMVar mv $
                unlift $
                viewObjectPushEdit $ \_ push -> do
                    run <- getSequenceRun iter1 iter2
                    push $ pure $ StringReplaceSection run mempty
    widget <- new TextView [#buffer := buffer]
    cvReceiveUpdate $ \_ _ edit ->
        liftIO $
        ifMVar mv $
        case edit of
            StringReplaceWhole text -> #setText buffer text (-1)
            StringReplaceSection bounds text -> replaceText buffer bounds text
    let
        aspect :: Aspect (EditLens (StringEdit Text) (StringEdit Text))
        aspect = do
            (_, iter1, iter2) <- #getSelectionBounds buffer
            run <- getSequenceRun iter1 iter2
            -- get selection...
            lens <- stringSectionLens run
            return $ Just lens
    cvAddAspect aspect
    _ <-
        cvLiftView $
        liftIOView $ \unlift ->
            on widget #focus $ \_ ->
                unlift $ do
                    viewSetSelection aspect
                    return True
    toWidget widget

textAreaGetView :: GetGView
textAreaGetView = MkGetView $ \_ uispec -> fmap (\MkTextAreaUISpec -> textView) $ isUISpec uispec
