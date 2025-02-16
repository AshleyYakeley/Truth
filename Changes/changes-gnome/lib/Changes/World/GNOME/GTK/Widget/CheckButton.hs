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
    gvRunLockedThen $ do
        (button, widget) <- gvNewWidget GI.CheckButton [#active GI.:= initial]
        changedSignal <-
            gvOnSignal button #toggled $ do
                st <- gvLiftIO $ GI.get button #active
                _ <- gvRunUnlocked $ gvSetWholeModel rmod esrc st
                return ()
        return $ do
            gvBindReadOnlyWholeModel label $ \val -> gvRunLocked $ GI.set button [#label GI.:= val]
            gvBindWholeModel rmod (Just esrc) $ \st ->
                gvRunLocked $ withSignalBlocked button changedSignal $ GI.set button [#active GI.:= st]
            return widget

createMaybeCheckButton :: Model (ROWUpdate Text) -> Model (WholeUpdate (Maybe Bool)) -> GView 'Unlocked GI.Widget
createMaybeCheckButton label rmod = do
    initial <- gvLiftView $ viewRunResource rmod $ \asub -> aModelRead asub ReadWhole
    gvRunLockedThen $ do
        (button, widget) <-
            gvNewWidget GI.CheckButton [#active GI.:= initial == Just True, #inconsistent GI.:= initial == Nothing]
        let
            getWidgetState :: GView 'Locked (Maybe Bool)
            getWidgetState = do
                active <- GI.get button #active
                inconsistent <- GI.get button #inconsistent
                return
                    $ if inconsistent
                        then Nothing
                        else Just active
            setWidgetState :: Maybe Bool -> GView 'Locked ()
            setWidgetState st = GI.set button [#active GI.:= st == Just True, #inconsistent GI.:= st == Nothing]
        clickGesture <- gvNew GI.GestureClick []
        #addController button clickGesture
        _ <-
            gvOnSignal clickGesture #released $ \_ _ _ -> do
                mevent <- #getCurrentEvent clickGesture
                for_ mevent $ \event -> do
                    modifiers <- #getModifierState event
                    oldst <- getWidgetState
                    let
                        newst =
                            if elem GI.ModifierTypeShiftMask modifiers
                                then Nothing
                                else Just (oldst /= Just True)
                    _ <- gvRunUnlocked $ gvSetWholeModel rmod noEditSource newst
                    return ()
        return $ do
            gvBindReadOnlyWholeModel label $ \val -> gvRunLocked $ GI.set button [#label GI.:= val]
            gvBindWholeModel rmod Nothing $ \mb -> gvRunLocked $ setWidgetState mb
            return widget
