module Truth.UI.GTK.Entry
    ( textEntryGetView
    ) where

import GI.Gtk as Gtk
import Shapes hiding (get)
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful

textEntryGetView :: GetGView
textEntryGetView =
    MkGetView $ \_ uispec ->
        fmap
            (\MkUITextEntry -> do
                 widget <- new Entry []
                 initial <- liftOuter $ viewObjectRead mutableReadToSubject
                 set widget [#text := initial]
                 changedSignal <-
                     liftOuter $
                     viewOn widget #changed $
                     viewObjectPushEdit $ \push -> do
                         st <- get widget #text
                         push [MkWholeEdit st]
                 createViewReceiveUpdate $ \_ (MkWholeEdit st) ->
                     liftIO $ withSignalBlocked widget changedSignal $ set widget [#text := st]
                 toWidget widget) $
        isUISpec uispec
