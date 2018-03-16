module Truth.UI.GTK.CheckButton
    ( checkButtonGetView
    ) where

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

checkButtonGetView :: GetGView
checkButtonGetView = MkGetView $ \_ uispec -> fmap createWidget $ isUISpec uispec
