module Changes.UI.GTK.CheckButton
    ( createCheckButton
    , createMaybeCheckButton
    ) where

import Changes.Core
import Changes.GI
import GI.Gdk
import GI.Gtk as Gtk
import Shapes

createCheckButton :: Model (ROWUpdate Text) -> Model (WholeUpdate Bool) -> View Widget
createCheckButton label rmod = do
    esrc <- newEditSource
    initial <- viewRunResource rmod $ \asub -> aModelRead asub ReadWhole
    widget <- cvNew CheckButton [#active := initial]
    viewBindReadOnlyWholeModel label $ \val -> set widget [#label := val]
    changedSignal <-
        viewOn widget #clicked $
        viewRunResource rmod $ \asub -> do
            st <- Gtk.get widget #active
            _ <- pushEdit esrc $ aModelEdit asub $ pure $ MkWholeReaderEdit st
            return ()
    viewBindWholeModel rmod (Just esrc) $ \st -> withSignalBlocked widget changedSignal $ set widget [#active := st]
    toWidget widget

createMaybeCheckButton :: Model (ROWUpdate Text) -> Model (WholeUpdate (Maybe Bool)) -> View Widget
createMaybeCheckButton label rmod = do
    initial <- viewRunResource rmod $ \asub -> aModelRead asub ReadWhole
    widget <- cvNew CheckButton [#active := initial == Just True, #inconsistent := initial == Nothing]
    viewBindReadOnlyWholeModel label $ \val -> set widget [#label := val]
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
        viewOn widget #buttonPressEvent $ \event -> do
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
    viewBindWholeModel rmod Nothing setWidgetState
    toWidget widget
