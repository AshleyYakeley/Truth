module Truth.UI.GTK.Entry
    ( textEntryGetView
    ) where

import GI.Gdk
import GI.Gtk as Gtk
import Shapes hiding (get)
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful

textEntryGetView :: GetGView
textEntryGetView =
    MkGetView $ \_ uispec ->
        fmap
            (\(MkTextEntryUISpec sub) -> do
                 esrc <- newEditSource
                 initial <- cvLiftView $ viewObjectRead sub $ \_ -> mutableReadToSubject
                 widget <- new Entry [#text := initial]
                 invalidCol <- new RGBA [#red := 1, #green := 0, #blue := 0, #alpha := 1]
                 let
                     setValidState ::
                            forall m. MonadIO m
                         => Bool
                         -> m ()
                     setValidState True = #overrideColor widget [StateFlagsNormal] Nothing
                     setValidState False = #overrideColor widget [StateFlagsNormal] $ Just invalidCol
                 changedSignal <-
                     cvLiftView $
                     viewOn widget #changed $
                     viewObjectPushEdit sub $ \_ push -> do
                         st <- get widget #text
                         succeeded <- push esrc $ pure $ MkWholeReaderEdit st
                         setValidState succeeded
                 cvReceiveUpdate sub (Just esrc) $ \_ _ (MkWholeReaderUpdate newtext) ->
                     liftIO $
                     withSignalBlocked widget changedSignal $ do
                         oldtext <- get widget #text
                         if oldtext == newtext
                             then return ()
                             else set widget [#text := newtext]
                         setValidState True
                 toWidget widget) $
        isUISpec uispec
