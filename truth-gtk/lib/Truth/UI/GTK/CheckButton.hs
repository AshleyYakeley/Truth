module Truth.UI.GTK.CheckButton
    ( checkButtonGetView
    ) where

import GI.Gdk
import GI.Gtk as Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful

createWidget :: CheckboxUISpec -> CreateView Widget
createWidget (MkCheckboxUISpec label rmod) = do
    esrc <- newEditSource
    initial <- cvRunResource rmod $ \asub -> subRead asub ReadWhole
    widget <- new CheckButton [#active := initial]
    cvBindReadOnlyWholeSubscriber label $ \val -> set widget [#label := val]
    changedSignal <-
        cvOn widget #clicked $
        viewRunResource rmod $ \asub -> do
            st <- Gtk.get widget #active
            _ <- pushEdit esrc $ subEdit asub $ pure $ MkWholeReaderEdit st
            return ()
    cvBindWholeSubscriber rmod (Just esrc) $ \st ->
        liftIO $ withSignalBlocked widget changedSignal $ set widget [#active := st]
    toWidget widget
createWidget (MkMaybeCheckboxUISpec label rmod) = do
    initial <- cvRunResource rmod $ \asub -> subRead asub ReadWhole
    widget <- new CheckButton [#active := initial == Just True, #inconsistent := initial == Nothing]
    cvBindReadOnlyWholeSubscriber label $ \val -> set widget [#label := val]
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
        cvOn widget #buttonPressEvent $ \event -> do
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
                            pushEdit noEditSource $ subEdit asub $ pure $ MkWholeReaderEdit newst
                    return True
                _ -> return False
    cvBindWholeSubscriber rmod Nothing setWidgetState
    toWidget widget

checkButtonGetView :: GetGView
checkButtonGetView = MkGetView $ \_ uispec -> fmap createWidget $ isUISpec uispec
