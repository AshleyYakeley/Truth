module Truth.UI.GTK.CheckButton
    ( checkButtonGetView
    ) where

import GI.Gdk
import GI.Gtk as Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful

createWidget :: CheckboxUISpec sel edit -> CreateView sel edit Widget
createWidget (MkCheckboxUISpec label lens) = do
    esrc <- newEditSource
    initial <- cvLiftView $ viewMapEdit lens $ viewObjectRead $ \_ -> mutableReadToSubject
    widget <- new CheckButton [#active := initial]
    cvBindEditFunction Nothing label $ \val -> set widget [#label := val]
    cvMapEdit lens $ do
        _ <-
            cvLiftView $
            viewOn widget #clicked $
            viewObjectPushEdit $ \_ push -> do
                st <- Gtk.get widget #active
                _ <- push noEditSource [MkWholeEdit st]
                return ()
        cvBindEditFunction (Just esrc) id $ \st -> liftIO $ set widget [#active := st]
    toWidget widget
createWidget (MkMaybeCheckboxUISpec label lens) = do
    initial <- cvLiftView $ viewMapEdit lens $ viewObjectRead $ \_ -> mutableReadToSubject
    widget <- new CheckButton [#active := initial == Just True, #inconsistent := initial == Nothing]
    cvBindEditFunction Nothing label $ \val -> set widget [#label := val]
    cvMapEdit lens $ do
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
                                _ <- push noEditSource [MkWholeEdit newst]
                                return True
                            _ -> return False
        cvBindEditFunction Nothing id setWidgetState
    toWidget widget

checkButtonGetView :: GetGView
checkButtonGetView = MkGetView $ \_ uispec -> fmap createWidget $ isUISpec uispec
