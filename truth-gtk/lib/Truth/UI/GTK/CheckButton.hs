module Truth.UI.GTK.CheckButton
    ( checkButtonGetView
    ) where

import GI.Gdk
import GI.Gtk as Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful

createWidget :: CheckboxUISpec sel -> CreateView sel Widget
createWidget (MkCheckboxUISpec label sub) =
    runResource sub $ \run asub -> do
        esrc <- newEditSource
        initial <- liftIO $ run $ subRead asub ReadWhole
        widget <- new CheckButton [#active := initial]
        cvBindUpdateFunction Nothing label $ \val -> set widget [#label := val]
        changedSignal <-
            cvLiftView $
            viewOn widget #clicked $
            liftIO $
            run $ do
                st <- Gtk.get widget #active
                _ <- pushEdit noEditSource $ subEdit asub $ pure $ MkWholeReaderEdit st
                return ()
        cvBindUpdateFunction (Just esrc) (subscriberToReadOnly sub) $ \st ->
            liftIO $ withSignalBlocked widget changedSignal $ set widget [#active := st]
        toWidget widget
createWidget (MkMaybeCheckboxUISpec label sub) =
    runResource sub $ \run asub -> do
        initial <- liftIO $ run $ subRead asub ReadWhole
        widget <- new CheckButton [#active := initial == Just True, #inconsistent := initial == Nothing]
        cvBindUpdateFunction Nothing label $ \val -> set widget [#label := val]
        let
            getWidgetState ::
                   forall m. MonadIO m
                => m (Maybe Bool)
            getWidgetState = do
                active <- Gtk.get widget #active
                inconsistent <- Gtk.get widget #inconsistent
                return $
                    if inconsistent
                        then Nothing
                        else Just active
            setWidgetState ::
                   forall m. MonadIO m
                => Maybe Bool
                -> m ()
            setWidgetState st = set widget [#active := st == Just True, #inconsistent := st == Nothing]
        _ <-
            liftIO $
            on widget #buttonPressEvent $ \event ->
                run $ do
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
                            _ <- pushEdit noEditSource $ subEdit asub $ pure $ MkWholeReaderEdit newst
                            return True
                        _ -> return False
        cvBindUpdateFunction Nothing (subscriberToReadOnly sub) setWidgetState
        toWidget widget

checkButtonGetView :: GetGView
checkButtonGetView = MkGetView $ \_ uispec -> fmap createWidget $ isUISpec uispec
