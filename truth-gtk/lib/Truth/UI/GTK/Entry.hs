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
                 initial <- liftOuter $ viewMutableRead $ unReadable subjectFromReader
                 liftIO $ set widget [entryText := initial]
                 changedSignal <-
                     liftOuter $
                     viewOn widget editableChanged $
                     viewMutableEdit $ \muted -> do
                         st <- liftIO $ Gtk.get widget entryText
                         pushMutableEdit muted [MkWholeEdit st]
                 createViewReceiveUpdate $ \_ (MkWholeEdit st) ->
                     liftIO $ withSignalBlocked changedSignal $ set widget [entryText := st]
                 return $ toWidget widget) $
        isUISpec uispec
