module Changes.World.GNOME.GTK.Widget.TextEntry
    ( BadTextBehaviour (..)
    , createTextEntry
    , testTextEntry
    )
where

import Data.Int
import Shapes.Test

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

data BadTextBehaviour = PreventBadText | MarkBadText

entryEditable :: GI.Entry -> GView 'Locked GI.Editable
entryEditable entry = do
    mdelegate <- GI.editableGetDelegate entry
    maybe (GI.toEditable entry) return mdelegate

attachEntryText :: BadTextBehaviour -> GI.Entry -> Model (WholeUpdate Text) -> GView 'Unlocked ()
attachEntryText btb entry rmod = do
    esrc <- gvNewEditSource
    gvRunLockedThen $ mdo
        editable <- entryEditable entry
        let
            setValidState :: Bool -> GView 'Locked ()
            setValidState True = #removeCssClass entry "error"
            setValidState False = #addCssClass entry "error"
            setEntryText :: Text -> GView 'Locked ()
            setEntryText newtext = do
                oldtext <- GI.editableGetText editable
                if oldtext == newtext
                    then return ()
                    else GI.editableSetText editable newtext
            textPosition :: Int32 -> Text -> SequencePoint
            textPosition pos text = clipPoint (seqLength text) $ fromIntegral pos
            insertEntryText :: Int32 -> Text -> Text -> Text
            insertEntryText pos inserted oldtext = let
                p = textPosition pos oldtext
                in seqTake p oldtext <> inserted <> seqDrop p oldtext
            deleteEntryText :: Int32 -> Int32 -> Text -> Text
            deleteEntryText startPos endPos oldtext = let
                start = textPosition startPos oldtext
                end =
                    if endPos < 0
                        then seqLength oldtext
                        else textPosition endPos oldtext
                low = min start end
                high = max start end
                in seqTake low oldtext <> seqDrop high oldtext
            blockSignals :: GView 'Locked () -> GView 'Locked ()
            blockSignals = withSignalsBlocked editable [insertSignal, deleteSignal]
            pushEntryText :: Text -> Text -> GView 'Locked ()
            pushEntryText signalName newtext = do
                succeeded <- gvRunUnlocked $ gvSetWholeModel rmod esrc newtext
                case btb of
                    PreventBadText -> unless succeeded $ GI.signalStopEmissionByName editable signalName
                    MarkBadText -> setValidState succeeded
        insertSignal <-
            gvOnSignal 0 editable #insertText $ \inserted _ position -> do
                oldtext <- GI.editableGetText editable
                pushEntryText "insert-text" $ insertEntryText position inserted oldtext
                return position
        deleteSignal <-
            gvOnSignal () editable #deleteText $ \startPos endPos -> do
                oldtext <- GI.editableGetText editable
                pushEntryText "delete-text" $ deleteEntryText startPos endPos oldtext
                return ()
        return $ do
            gvBindWholeModel rmod (Just esrc) $ \newtext ->
                gvRunLocked
                    $ blockSignals
                    $ do
                        setEntryText newtext
                        setValidState True

createEntry :: GView 'Locked (GI.Entry, GI.Widget)
createEntry = gvNewWidget GI.Entry []

createTextEntry :: BadTextBehaviour -> Model (WholeUpdate Text) -> GView 'Unlocked GI.Widget
createTextEntry btb rmod = do
    (entry, widget) <- gvRunLocked $ gvNewWidget GI.Entry []
    attachEntryText btb entry rmod
    return widget

rejectingWholeTextModel :: Text -> Model (WholeUpdate Text)
rejectingWholeTextModel text = mapModel fromReadOnlyRejectingChangeLens $ constantModel text

testTextEntry :: GView 'Unlocked ()
testTextEntry = do
    (entry, _) <- gvRunLocked createEntry
    attachEntryText PreventBadText entry $ rejectingWholeTextModel "constant"
    gvRunLocked $ do
        editable <- entryEditable entry
        _ <- #insertText editable "changed" (-1) 0
        textAfterInsert <- #getText editable
        gvLiftIO $ assertEqual "entry text after insert" "constant" textAfterInsert
        #deleteText editable 0 1
        text <- #getText editable
        gvLiftIO $ assertEqual "entry text after delete" "constant" text
