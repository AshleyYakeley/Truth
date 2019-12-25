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

textView :: Subscriber (StringUpdate Text) -> GCreateView TextSelection
textView sub =
    runResource sub $ \run asub -> do
        esrc <- newEditSource
        buffer <- new TextBuffer []
        initial <- liftIO $ run $ mutableReadToSubject $ subRead asub
        #setText buffer initial (-1)
        insertSignal <-
            liftIO $
            on buffer #insertText $ \iter text _ ->
                run $ do
                    p <- getSequencePoint iter
                    _ <- pushEdit esrc $ subEdit asub $ pure $ StringReplaceSection (MkSequenceRun p 0) text
                    return ()
        deleteSignal <-
            liftIO $
            on buffer #deleteRange $ \iter1 iter2 ->
                run $ do
                    srun <- getSequenceRun iter1 iter2
                    _ <- pushEdit esrc $ subEdit asub $ pure $ StringReplaceSection srun mempty
                    return ()
        widget <- new TextView [#buffer := buffer]
        cvReceiveUpdate sub (Just esrc) $ \(MkEditUpdate edit) ->
            withSignalBlocked buffer insertSignal $
            withSignalBlocked buffer deleteSignal $
            case edit of
                StringReplaceWhole text -> #setText buffer text (-1)
                StringReplaceSection bounds text -> replaceText buffer bounds text
        let
            aspect :: Aspect TextSelection
            aspect = do
                (_, iter1, iter2) <- #getSelectionBounds buffer
            -- get selection...
                srun <- getSequenceRun iter1 iter2
                return $ Just $ stringSectionLens srun
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
textAreaGetView = MkGetView $ \_ uispec -> fmap (\(MkTextAreaUISpec sub) -> textView sub) $ isUISpec uispec
