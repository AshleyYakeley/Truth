module Changes.UI.GTK.Text
    ( TextSelection
    , createTextArea
    ) where

import Changes.Core
import Changes.GI
import GI.Gtk
import Shapes

type TextSelection = FloatingChangeLens (StringUpdate Text) (StringUpdate Text)

replaceText :: TextBuffer -> SequenceRun -> Text -> View ()
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

getSequencePoint :: MonadIO m => TextIter -> m SequencePoint
getSequencePoint iter = do
    p <- #getOffset iter
    return $ MkSequencePoint $ fromIntegral p

getSequenceRun :: MonadIO m => TextIter -> TextIter -> m SequenceRun
getSequenceRun iter1 iter2 = do
    p1 <- getSequencePoint iter1
    p2 <- getSequencePoint iter2
    return $ startEndRun p1 p2

createTextArea :: Model (StringUpdate Text) -> SelectNotify TextSelection -> View Widget
createTextArea rmod (MkSelectNotify setsel) = do
    esrc <- newEditSource
    buffer <- cvNew TextBuffer []
    insertSignal <-
        viewOn buffer #insertText $ \iter text _ -> do
            p <- getSequencePoint iter
            liftIO $
                runResource emptyResourceContext rmod $ \asub -> do
                    _ <- pushEdit esrc $ aModelEdit asub $ pure $ StringReplaceSection (MkSequenceRun p 0) text
                    return ()
    deleteSignal <-
        viewOn buffer #deleteRange $ \iter1 iter2 -> do
            srun <- getSequenceRun iter1 iter2
            liftIO $
                runResource emptyResourceContext rmod $ \asub -> do
                    _ <- pushEdit esrc $ aModelEdit asub $ pure $ StringReplaceSection srun mempty
                    return ()
    let
        getSelection :: View SequenceRun
        getSelection = do
            (_, iter1, iter2) <- #getSelectionBounds buffer
            getSequenceRun iter1 iter2
        aspect :: View (Maybe TextSelection)
        aspect = do
            srun <- getSelection
            return $ Just $ stringSectionLens srun
    setsel aspect
    _ <- viewAfter buffer #changed $ setsel aspect
    _ <- viewAfter buffer #markSet $ \_ _ -> setsel aspect
    let
        initV :: View ()
        initV = do
            initial <- viewRunResource rmod $ \am -> readableToSubject $ aModelRead am
            withSignalsBlocked buffer [insertSignal, deleteSignal] $ #setText buffer initial (-1)
        recvV :: () -> NonEmpty (StringUpdate Text) -> View ()
        recvV () updates =
            for_ updates $ \(MkEditUpdate edit) ->
                withSignalsBlocked buffer [insertSignal, deleteSignal] $
                case edit of
                    StringReplaceWhole text -> #setText buffer text (-1)
                    StringReplaceSection bounds text -> replaceText buffer bounds text
    viewBindModel rmod (Just esrc) initV mempty recvV
    widget <- cvNew TextView [#buffer := buffer]
    toWidget widget
