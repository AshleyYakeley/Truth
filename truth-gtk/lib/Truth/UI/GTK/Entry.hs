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
                         _ <- traceBracketArgs "GTK.TextEntry:push" (show st) show $ push [MkWholeEdit st]
                         return ()
                 cvReceiveUpdate $ \_ _ (MkWholeEdit newtext) -> traceBracketArgs "GTK.TextEntry:update" (show newtext) show $
                     liftIO $
                     withSignalBlocked widget changedSignal $ do
                         oldtext <- get widget #text
                         if oldtext == newtext
                             then return ()
                             else set widget [#text := newtext]
                 toWidget widget) $
        isUISpec uispec
