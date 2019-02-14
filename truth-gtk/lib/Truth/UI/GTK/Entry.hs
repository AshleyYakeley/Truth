module Truth.UI.GTK.Entry
    ( textEntryGetView
    ) where

import GI.Gtk as Gtk
import Shapes hiding (get)
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful
import Truth.Debug.Object

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
                     traceBracket "GTK.TextEntry:changed" $
                     viewObjectPushEdit $ \_ push -> do
                         st <- get widget #text
                         traceBracketArgs "GTK.TextEntry:push" (show st) show $ push [MkWholeEdit st]
                 cvReceiveUpdate $ \_ _ (MkWholeEdit st) -> traceBracketArgs "GTK.TextEntry:update" (show st) show $
                     liftIO $ withSignalBlocked widget changedSignal $ set widget [#text := st]
                 toWidget widget) $
        isUISpec uispec
