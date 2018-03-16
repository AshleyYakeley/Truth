module Truth.UI.GTK.CheckButton
    ( checkButtonGetView
    ) where

import GI.Gdk
import GI.Gtk as Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful

createWidget :: UICheckbox edit -> CreateView edit Widget
createWidget (MkUICheckbox name) = do
    initial <- cvLiftView $ viewObjectRead $ \_ -> mutableReadToSubject
    widget <- new CheckButton [#label := name, #active := initial]
    changedSignal <-
        cvLiftView $
        viewOn widget #clicked $
        viewObjectPushEdit $ \_ push -> do
            st <- Gtk.get widget #active
            push [MkWholeEdit st]
    cvBindEditFunction id $ \st -> liftIO $ withSignalBlocked widget changedSignal $ set widget [#active := st]
    toWidget widget
createWidget (MkUIMaybeCheckbox name) = do
    initial <- cvLiftView $ viewObjectRead $ \_ -> mutableReadToSubject
    widget <- new CheckButton [#label := name, #active := initial == Just True, #inconsistent := initial == Nothing]
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
                viewObjectPushEdit $ \_ push -> do
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
                            push [MkWholeEdit newst]
                            return True
                        _ -> return False
    cvBindEditFunction id setWidgetState
    toWidget widget

checkButtonGetView :: GetGView
checkButtonGetView = MkGetView $ \_ uispec -> fmap createWidget $ isUISpec uispec
