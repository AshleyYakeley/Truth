module Changes.World.GNOME.GTK.Element.CheckButton
    ( createCheckButton
    , createMaybeCheckButton
    ) where

import Changes.Core
import Changes.World.GNOME.GI
import GI.Gdk
import GI.Gtk as Gtk
import Shapes

createCheckButton :: Model (ROWUpdate Text) -> Model (WholeUpdate Bool) -> GView 'Unlocked Widget
createCheckButton label rmod = do
    esrc <- gvNewEditSource
    initial <- gvLiftView $ viewRunResource rmod $ \asub -> aModelRead asub ReadWhole
    gvRunLockedThen $ do
        (button, widget) <- gvNewWidget CheckButton [#active := initial]
        changedSignal <-
            gvOnSignal button #clicked $ do
                st <- gvLiftIO $ Gtk.get button #active
                gvRunUnlocked $
                    gvLiftView $
                    viewRunResource rmod $ \asub -> do
                        _ <- pushEdit esrc $ aModelEdit asub $ pure $ MkWholeReaderEdit st
                        return ()
        return $ do
            gvBindReadOnlyWholeModel label $ \val -> gvRunLocked $ set button [#label := val]
            gvBindWholeModel rmod (Just esrc) $ \st ->
                gvRunLocked $ withSignalBlocked button changedSignal $ set button [#active := st]
            return widget

createMaybeCheckButton :: Model (ROWUpdate Text) -> Model (WholeUpdate (Maybe Bool)) -> GView 'Unlocked Widget
createMaybeCheckButton label rmod = do
    initial <- gvLiftView $ viewRunResource rmod $ \asub -> aModelRead asub ReadWhole
    gvRunLockedThen $ do
        (button, widget) <-
            gvNewWidget CheckButton [#active := initial == Just True, #inconsistent := initial == Nothing]
        let
            getWidgetState :: GView 'Locked (Maybe Bool)
            getWidgetState = do
                active <- Gtk.get button #active
                inconsistent <- Gtk.get button #inconsistent
                return $
                    if inconsistent
                        then Nothing
                        else Just active
            setWidgetState :: Maybe Bool -> GView 'Locked ()
            setWidgetState st = set button [#active := st == Just True, #inconsistent := st == Nothing]
        _ <-
            gvOnSignal button #buttonPressEvent $ \event -> do
                click <- Gtk.get event #type
                case click of
                    EventTypeButtonPress -> do
                        modifiers <- Gtk.get event #state
                        oldst <- getWidgetState
                        let
                            newst =
                                if elem ModifierTypeShiftMask modifiers
                                    then Nothing
                                    else Just (oldst /= Just True)
                        _ <-
                            gvRunUnlocked $
                            gvLiftView $
                            viewRunResource rmod $ \asub ->
                                pushEdit noEditSource $ aModelEdit asub $ pure $ MkWholeReaderEdit newst
                        return True
                    _ -> return False
        return $ do
            gvBindReadOnlyWholeModel label $ \val -> gvRunLocked $ set button [#label := val]
            gvBindWholeModel rmod Nothing $ \mb -> gvRunLocked $ setWidgetState mb
            return widget
