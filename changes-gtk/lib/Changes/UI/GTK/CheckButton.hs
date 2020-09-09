module Truth.UI.GTK.CheckButton
    ( createCheckButton
    , createMaybeCheckButton
    ) where

import GI.Gdk
import GI.Gtk as Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.Useful
import Truth.Debug.Reference

createCheckButton :: Model (ROWUpdate Text) -> Model (WholeUpdate Bool) -> CreateView Widget
createCheckButton label rmod = do
    esrc <- newEditSource
    initial <- viewRunResource rmod $ \asub -> aModelRead asub ReadWhole
    widget <- cvNew CheckButton [#active := initial]
    cvBindReadOnlyWholeModel label $ \val -> set widget [#label := val]
    changedSignal <-
        cvOn widget #clicked $
        traceBracket "GTK.CheckButton:clicked" $
        viewRunResource rmod $ \asub -> do
            st <- Gtk.get widget #active
            _ <- pushEdit esrc $ aModelEdit asub $ pure $ MkWholeReaderEdit st
            return ()
    cvBindWholeModel rmod (Just esrc) $ \st -> withSignalBlocked widget changedSignal $ set widget [#active := st]
    toWidget widget

createMaybeCheckButton :: Model (ROWUpdate Text) -> Model (WholeUpdate (Maybe Bool)) -> CreateView Widget
createMaybeCheckButton label rmod = do
    initial <- viewRunResource rmod $ \asub -> aModelRead asub ReadWhole
    widget <- cvNew CheckButton [#active := initial == Just True, #inconsistent := initial == Nothing]
    cvBindReadOnlyWholeModel label $ \val -> set widget [#label := val]
    let
        getWidgetState :: View (Maybe Bool)
        getWidgetState = do
            active <- Gtk.get widget #active
            inconsistent <- Gtk.get widget #inconsistent
            return $
                if inconsistent
                    then Nothing
                    else Just active
        setWidgetState :: Maybe Bool -> View ()
        setWidgetState st = set widget [#active := st == Just True, #inconsistent := st == Nothing]
    _ <-
        cvOn widget #buttonPressEvent $ \event -> traceBracket "GTK.CheckButton:buttonPressEvent" $ do
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
                        viewRunResource rmod $ \asub ->
                            pushEdit noEditSource $ aModelEdit asub $ pure $ MkWholeReaderEdit newst
                    return True
                _ -> return False
    cvBindWholeModel rmod Nothing setWidgetState
    toWidget widget
