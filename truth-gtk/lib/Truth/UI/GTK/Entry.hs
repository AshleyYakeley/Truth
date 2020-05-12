module Truth.UI.GTK.Entry
    ( textEntryGetView
    ) where

import GI.Gdk
import GI.Gtk as Gtk
import Shapes hiding (get)
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful
import Truth.Debug.Reference

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
                     viewRunResource rmod $ \asub -> traceBracket "GTK.TextEntry:changed:run" $ do
                         st <- traceBracket "GTK.TextEntry:changed:get" $ get widget #text
                         succeeded <- traceBracketArgs "GTK.TextEntry:push" (show st) show $ pushEdit esrc $ aModelEdit asub $ pure $ MkWholeReaderEdit st
                         traceBracket "GTK.TextEntry:changed:setValidState" $ setValidState succeeded
                 cvBindWholeModel rmod (Just esrc) $ \newtext ->
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
