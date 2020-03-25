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

textView :: Subscriber (StringUpdate Text) -> SelectNotify TextSelection -> GCreateView
textView rmod (MkSelectNotify setsel) = do
    esrc <- newEditSource
    buffer <- new TextBuffer []
    insertSignal <-
        cvOn buffer #insertText $ \iter text _ -> do
            p <- getSequencePoint iter
            liftIO $
                runResource emptyResourceContext rmod $ \asub -> do
                    _ <- pushEdit esrc $ subEdit asub $ pure $ StringReplaceSection (MkSequenceRun p 0) text
                    return ()
    deleteSignal <-
        cvOn buffer #deleteRange $ \iter1 iter2 -> do
            srun <- getSequenceRun iter1 iter2
            liftIO $
                runResource emptyResourceContext rmod $ \asub -> do
                    _ <- pushEdit esrc $ subEdit asub $ pure $ StringReplaceSection srun mempty
                    return ()
    let
        aspect :: View (Maybe TextSelection)
        aspect = do
            (_, iter1, iter2) <- #getSelectionBounds buffer
            -- get selection...
            srun <- getSequenceRun iter1 iter2
            return $ Just $ stringSectionLens srun
    cvLiftView $ setsel aspect
    _ <- cvOn buffer #changed $ setsel aspect
    let
        initV :: Subscriber (StringUpdate Text) -> CreateView ()
        initV rm = do
            initial <- viewRunResource rm $ \am -> readableToSubject $ subRead am
            liftIO $
                withSignalBlocked buffer insertSignal $
                withSignalBlocked buffer deleteSignal $ #setText buffer initial (-1)
        recvV :: () -> NonEmpty (StringUpdate Text) -> View ()
        recvV () updates =
            liftIO $
            for_ updates $ \(MkEditUpdate edit) ->
                withSignalBlocked buffer insertSignal $
                withSignalBlocked buffer deleteSignal $
                case edit of
                    StringReplaceWhole text -> #setText buffer text (-1)
                    StringReplaceSection bounds text -> replaceText buffer bounds text
    cvBindSubscriber rmod (Just esrc) initV mempty recvV
    widget <- new TextView [#buffer := buffer]
    toWidget widget

textAreaGetView :: GetGView
textAreaGetView =
    MkGetView $ \_ uispec -> fmap (\(MkTextAreaUISpec sub setsel) -> textView sub setsel) $ isUISpec uispec
