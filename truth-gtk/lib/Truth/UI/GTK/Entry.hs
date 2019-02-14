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
            (\MkTextAreaUISpecEntry -> do
                 initial <- cvLiftView $ viewObjectRead $ \_ -> mutableReadToSubject
                 widget <- new Entry [#text := initial]
                 changedSignal <-
                     cvLiftView $
                     viewOn widget #changed $
                     viewObjectPushEdit $ \_ push -> do
                         st <- get widget #text
                         push [MkWholeEdit st]
                 cvReceiveUpdate $ \_ _ (MkWholeEdit st) ->
                     liftIO $ withSignalBlocked widget changedSignal $ set widget [#text := st]
                 toWidget widget) $
        isUISpec uispec
