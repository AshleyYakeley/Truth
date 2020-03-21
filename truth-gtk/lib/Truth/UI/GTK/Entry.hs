module Truth.UI.GTK.Entry
    ( textEntryGetView
    ) where

import GI.Gdk
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
            (\(MkTextEntryUISpec rmod) -> do
                 esrc <- newEditSource
                 widget <- new Entry []
                 invalidCol <- new RGBA [#red := 1, #green := 0, #blue := 0, #alpha := 1]
                 let
                     setValidState ::
                            forall m. MonadIO m
                         => Bool
                         -> m ()
                     setValidState True = #overrideColor widget [StateFlagsNormal] Nothing
                     setValidState False = #overrideColor widget [StateFlagsNormal] $ Just invalidCol
                 changedSignal <-
                     cvOn widget #changed $
                     traceBracket "GTK.TextEntry:changed" $
                     viewRunResource rmod $ \asub -> do
                         st <- get widget #text
                         succeeded <- traceBracketArgs "GTK.TextEntry:push" (show st) show $ pushEdit esrc $ subEdit asub $ pure $ MkWholeReaderEdit st
                         setValidState succeeded
                 cvBindWholeSubscriber rmod (Just esrc) $ \newtext ->
                     traceBracketArgs "GTK.TextEntry:update" (show newtext) show $
                     liftIO $
                     withSignalBlocked widget changedSignal $ do
                         oldtext <- get widget #text
                         if oldtext == newtext
                             then return ()
                             else set widget [#text := newtext]
                         setValidState True
                 toWidget widget) $
        isUISpec uispec
