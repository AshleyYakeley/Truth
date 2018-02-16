module Truth.UI.GTK.CheckButton
    ( checkButtonGetView
    ) where

import GI.Gtk as Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful

checkButtonGetView :: GetGView
checkButtonGetView =
    MkGetView $ \_ uispec ->
        fmap
            (\(MkUICheckbox name) -> do
                 widget <- new CheckButton []
                 initial <- liftOuter $ viewObjectRead mutableReadToSubject
                 set widget [#label := name, #active := initial]
                 changedSignal <-
                     liftOuter $
                     viewOn widget #clicked $
                     viewObjectPushEdit $ \push -> do
                         st <- Gtk.get widget #active
                         push [MkWholeEdit st]
                 createViewReceiveUpdate $ \_ (MkWholeEdit st) ->
                     liftIO $ withSignalBlocked widget changedSignal $ set widget [#active := st]
                 toWidget widget) $
        isUISpec uispec
