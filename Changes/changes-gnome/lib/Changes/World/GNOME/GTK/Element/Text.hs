module Changes.World.GNOME.GTK.Element.Text
    ( TextSelection
    , createTextBuffer
    , createTextArea
    ) where

import Changes.Core
import Changes.World.GNOME.GI
import GI.Gtk
import Shapes

type TextSelection = FloatingChangeLens (StringUpdate Text) (StringUpdate Text)

replaceText :: TextBuffer -> SequenceRun -> Text -> GView 'Locked ()
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

getSequencePoint :: TextIter -> GView 'Locked SequencePoint
getSequencePoint iter = do
    p <- #getOffset iter
    return $ MkSequencePoint $ fromIntegral p

getSequenceRun :: TextIter -> TextIter -> GView 'Locked SequenceRun
getSequenceRun iter1 iter2 = do
    p1 <- getSequencePoint iter1
    p2 <- getSequencePoint iter2
    return $ startEndRun p1 p2

createTextBuffer :: Model (StringUpdate Text) -> SelectNotify TextSelection -> GView 'Unlocked TextBuffer
createTextBuffer rmod (MkSelectNotify setsel) = do
    esrc <- gvNewEditSource
    (buffer, blockSignals) <-
        gvRunLocked $ do
            buffer <- gvNew TextBuffer []
            insertSignal <-
                gvOnSignal buffer #insertText $ \iter text _ -> do
                    p <- getSequencePoint iter
                    gvLiftIO $
                        runResource emptyResourceContext rmod $ \asub -> do
                            _ <- pushEdit esrc $ aModelEdit asub $ pure $ StringReplaceSection (MkSequenceRun p 0) text
                            return ()
            deleteSignal <-
                gvOnSignal buffer #deleteRange $ \iter1 iter2 -> do
                    srun <- getSequenceRun iter1 iter2
                    gvLiftIO $
                        runResource emptyResourceContext rmod $ \asub -> do
                            _ <- pushEdit esrc $ aModelEdit asub $ pure $ StringReplaceSection srun mempty
                            return ()
            let
                getSelection :: GView 'Unlocked SequenceRun
                getSelection =
                    gvRunLocked $ do
                        (_, iter1, iter2) <- #getSelectionBounds buffer
                        getSequenceRun iter1 iter2
                aspect :: GView 'Unlocked (Maybe TextSelection)
                aspect = do
                    srun <- getSelection
                    return $ Just $ stringSectionLens srun
                setAspect :: GView 'Locked ()
                setAspect = gvRunUnlocked $ gvLiftViewWithUnlift $ \unlift -> setsel $ unlift aspect
                blockSignals :: GView 'Locked () -> GView 'Locked ()
                blockSignals = withSignalsBlocked buffer [insertSignal, deleteSignal]
            setAspect
            _ <- gvAfterSignal buffer #changed $ setAspect
            _ <- gvAfterSignal buffer #markSet $ \_ _ -> setAspect
            return (buffer, blockSignals)
    let
        initV :: GView 'Unlocked ()
        initV = do
            initial <- gvLiftView $ viewRunResource rmod $ \am -> readableToSubject $ aModelRead am
            gvRunLocked $ blockSignals $ #setText buffer initial (-1)
        recvV :: () -> NonEmpty (StringUpdate Text) -> GView 'Unlocked ()
        recvV () updates =
            gvRunLocked $
            blockSignals $
            for_ updates $ \(MkEditUpdate edit) ->
                case edit of
                    StringReplaceWhole text -> #setText buffer text (-1)
                    StringReplaceSection bounds text -> replaceText buffer bounds text
    gvBindModel rmod (Just esrc) initV mempty recvV
    return buffer

createTextArea :: Model (StringUpdate Text) -> SelectNotify TextSelection -> GView 'Unlocked Widget
createTextArea rmod seln = do
    buffer <- createTextBuffer rmod seln
    gvRunLocked $ do
        widget <- gvNew TextView [#buffer := buffer]
        toWidget widget
