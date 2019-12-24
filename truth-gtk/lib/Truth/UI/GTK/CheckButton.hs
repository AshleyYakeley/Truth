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
createWidget (MkCheckboxUISpec label sub) = do
    esrc <- newEditSource
    initial <- cvLiftView $ viewObjectRead sub $ \_ -> mutableReadToSubject
    widget <- new CheckButton [#active := initial]
    cvBindUpdateFunction Nothing label $ \val -> set widget [#label := val]
    changedSignal <-
        cvLiftView $
        viewOn widget #clicked $
        viewObjectPushEdit sub $ \_ push -> do
            st <- Gtk.get widget #active
            _ <- push noEditSource $ pure $ MkWholeReaderEdit st
            return ()
    cvBindUpdateFunction (Just esrc) (subscriberToReadOnly sub) $ \st ->
        liftIO $ withSignalBlocked widget changedSignal $ set widget [#active := st]
    toWidget widget
createWidget (MkMaybeCheckboxUISpec label sub) = do
    initial <- cvLiftView $ viewObjectRead sub $ \_ -> mutableReadToSubject
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
        cvLiftView $
        liftIOView $ \unlift ->
            on widget #buttonPressEvent $ \event ->
                unlift $
                viewObjectPushEdit sub $ \_ push -> do
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
                            _ <- push noEditSource $ pure $ MkWholeReaderEdit newst
                            return True
                        _ -> return False
    cvBindUpdateFunction Nothing (subscriberToReadOnly sub) setWidgetState
    toWidget widget

checkButtonGetView :: GetGView
checkButtonGetView = MkGetView $ \_ uispec -> fmap createWidget $ isUISpec uispec
