module Truth.UI.GTK.Entry
    ( textEntryGetView
    ) where

import Graphics.UI.Gtk as Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful

textEntryGetView :: GetGView
textEntryGetView =
    MkGetView $ \_ uispec ->
        fmap
            (\MkUITextEntry -> do
                 widget <- liftIO entryNew
                 initial <- liftOuter $ viewObjectRead mutableReadToSubject
                 liftIO $ set widget [entryText := initial]
                 changedSignal <-
                     liftOuter $
                     viewOn widget editableChanged $
                     viewObjectPushEdit $ \push -> do
                         st <- liftIO $ Gtk.get widget entryText
                         push [MkWholeEdit st]
                 createViewReceiveUpdate $ \_ (MkWholeEdit st) ->
                     liftIO $ withSignalBlocked changedSignal $ set widget [entryText := st]
                 return $ toWidget widget) $
        isUISpec uispec
