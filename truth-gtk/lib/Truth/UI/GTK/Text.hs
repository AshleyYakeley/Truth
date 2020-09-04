module Truth.UI.GTK.Text
    ( TextSelection
    , createTextArea
    ) where

import GI.Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.Useful
import Truth.Debug.Reference

type TextSelection = FloatingChangeLens (StringUpdate Text) (StringUpdate Text)

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

createTextArea :: Model (StringUpdate Text) -> SelectNotify TextSelection -> CreateView Widget
createTextArea rmod (MkSelectNotify setsel) = do
    esrc <- newEditSource
    buffer <- cvNew TextBuffer []
    insertSignal <-
        cvOn buffer #insertText $ \iter text _ -> traceBracket "GTK.Text:insert" $ do
            p <- getSequencePoint iter
            liftIO $
                runResource emptyResourceContext rmod $ \asub -> do
                    let edit = StringReplaceSection (MkSequenceRun p 0) text
                    _ <- traceBracket ("GTK.Text.insert: push " <> show edit) $ pushEdit esrc $ aModelEdit asub $ pure edit
                    return ()
    deleteSignal <-
        cvOn buffer #deleteRange $ \iter1 iter2 -> traceBracket "GTK.Text:delete" $ do
            srun <- getSequenceRun iter1 iter2
            liftIO $
                runResource emptyResourceContext rmod $ \asub -> do
                    let edit = StringReplaceSection srun mempty
                    _ <- traceBracket ("GTK.Text.delete: push " <> show edit) $ pushEdit esrc $ aModelEdit asub $ pure edit
                    return ()
    let
        aspect :: View (Maybe TextSelection)
        aspect = do
            (_, iter1, iter2) <- #getSelectionBounds buffer
            -- get selection...
            srun <- getSequenceRun iter1 iter2
            return $ Just $ stringSectionLens srun
    cvLiftView $ setsel aspect
    _ <- cvOn buffer #changed $ traceBracket "GTK.TextBuffer:changed" $ setsel aspect
    let
        initV :: CreateView ()
        initV = do
            initial <- viewRunResource rmod $ \am -> readableToSubject $ aModelRead am
            liftIO $
                withSignalBlocked buffer insertSignal $
                withSignalBlocked buffer deleteSignal $ #setText buffer initial (-1)
        recvV :: () -> NonEmpty (StringUpdate Text) -> View ()
        recvV () updates =
            liftIO $
            for_ updates $ \(MkEditUpdate edit) ->
                withSignalBlocked buffer insertSignal $
                withSignalBlocked buffer deleteSignal $
                traceBracket ("GTK.Text.receive: " <> show edit) $
                case edit of
                    StringReplaceWhole text -> #setText buffer text (-1)
                    StringReplaceSection bounds text -> replaceText buffer bounds text
    cvBindModel rmod (Just esrc) initV mempty recvV
    widget <- cvNew TextView [#buffer := buffer]
    toWidget widget
