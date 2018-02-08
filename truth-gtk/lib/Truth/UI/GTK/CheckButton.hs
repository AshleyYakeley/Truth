module Truth.UI.GTK.CheckButton
    ( checkButtonGetView
    ) where

import Graphics.UI.Gtk as Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful

checkButtonGetView :: GetGView
checkButtonGetView =
    MkGetView $ \_ uispec ->
        fmap
            (\(MkUICheckbox name) -> do
                 widget <- liftIO checkButtonNew
                 initial <- liftOuter $ viewObjectRead mutableReadToSubject
                 liftIO $ set widget [buttonLabel := name, toggleButtonActive := initial]
                 changedSignal <-
                     liftOuter $
                     viewOn widget buttonActivated $
                     viewObjectPushEdit $ \push -> do
                         st <- liftIO $ Gtk.get widget toggleButtonActive
                         push [MkWholeEdit st]
                 createViewReceiveUpdate $ \_ (MkWholeEdit st) ->
                     liftIO $ withSignalBlocked changedSignal $ set widget [toggleButtonActive := st]
                 return $ toWidget widget) $
        isUISpec uispec
