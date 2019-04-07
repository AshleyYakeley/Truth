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
                 esrc <- newEditSource
                 initial <- cvLiftView $ viewObjectRead $ \_ -> mutableReadToSubject
                 widget <- new Entry [#text := initial]
                 changedSignal <-
                     cvLiftView $
                     viewOn widget #changed $
                     viewObjectPushEdit $ \_ push -> do
                         st <- get widget #text
                         _ <- push esrc [MkWholeEdit st]
                         return ()
                 cvReceiveUpdate (Just esrc) $ \_ _ (MkWholeEdit newtext) ->
                     liftIO $
                     withSignalBlocked widget changedSignal $ do
                         oldtext <- get widget #text
                         if oldtext == newtext
                             then return ()
                             else set widget [#text := newtext]
                 toWidget widget) $
        isUISpec uispec
