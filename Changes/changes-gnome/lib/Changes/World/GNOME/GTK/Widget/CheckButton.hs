module Changes.World.GNOME.GTK.Widget.CheckButton
    ( createCheckButton
    , createMaybeCheckButton
    )
where

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

createCheckButton :: Model (ROWUpdate Text) -> Model (WholeUpdate Bool) -> GView 'Unlocked GI.Widget
createCheckButton label rmod = do
    esrc <- gvNewEditSource
    initial <- gvLiftView $ viewRunResource rmod $ \asub -> aModelRead asub ReadWhole
    gvRunLockedThen $ mdo
        (button, widget) <- gvNewWidget GI.CheckButton [#active GI.:= initial]
        let
            readModelState :: GView 'Locked Bool
            readModelState = gvRunUnlocked $ gvLiftView $ viewRunResource rmod $ \asub -> aModelRead asub ReadWhole
            setWidgetState :: Bool -> GView 'Locked ()
            setWidgetState st = GI.set button [#active GI.:= st]
            resetWidgetState :: GView 'Locked ()
            resetWidgetState = do
                st <- readModelState
                withSignalBlocked button changedSignal $ setWidgetState st
        changedSignal <-
            gvOnSignal () button #toggled $ do
                st <- gvLiftIO $ GI.get button #active
                success <- gvRunUnlocked $ gvSetWholeModel rmod esrc st
                unless success resetWidgetState
                return ()
        return $ do
            gvBindReadOnlyWholeModel label $ \val -> gvRunLocked $ GI.set button [#label GI.:= val]
            gvBindWholeModel rmod (Just esrc) $ \st ->
                gvRunLocked $ withSignalBlocked button changedSignal $ setWidgetState st
            return widget

createMaybeCheckButton :: Model (ROWUpdate Text) -> Model (WholeUpdate (Maybe Bool)) -> GView 'Unlocked GI.Widget
createMaybeCheckButton label rmod = do
    initial <- gvLiftView $ viewRunResource rmod $ \asub -> aModelRead asub ReadWhole
    gvRunLockedThen $ mdo
        (button, widget) <-
            gvNewWidget GI.CheckButton [#active GI.:= initial == Just True, #inconsistent GI.:= initial == Nothing]
        let
            readModelState :: GView 'Locked (Maybe Bool)
            readModelState = gvRunUnlocked $ gvLiftView $ viewRunResource rmod $ \asub -> aModelRead asub ReadWhole
            setWidgetState :: Maybe Bool -> GView 'Locked ()
            setWidgetState st = GI.set button [#active GI.:= st == Just True, #inconsistent GI.:= st == Nothing]
            resetWidgetState :: GView 'Locked ()
            resetWidgetState = do
                st <- readModelState
                withSignalBlocked button toggledSignal $ setWidgetState st
        toggledSignal <-
            gvOnSignal () button #toggled $ do
                active <- GI.get button #active
                success <- gvRunUnlocked $ gvSetWholeModel rmod noEditSource $ Just active
                unless success resetWidgetState
                return ()
        clickGesture <- gvNew GI.GestureClick []
        _ <-
            gvOnSignal () clickGesture #released $ \_ _ _ -> do
                modifiers <- #getCurrentEventState clickGesture
                if elem GI.ModifierTypeShiftMask modifiers
                    then do
                        success <- gvRunUnlocked $ gvSetWholeModel rmod noEditSource Nothing
                        unless success resetWidgetState
                        return ()
                    else return ()
                return ()
        clickGestureTransferred <- gvDuplicateUnbound GI.GestureClick clickGesture
        #addController button clickGestureTransferred
        return $ do
            gvBindReadOnlyWholeModel label $ \val -> gvRunLocked $ GI.set button [#label GI.:= val]
            gvBindWholeModel rmod Nothing $ \mb -> gvRunLocked $ withSignalBlocked button toggledSignal $ setWidgetState mb
            return widget
