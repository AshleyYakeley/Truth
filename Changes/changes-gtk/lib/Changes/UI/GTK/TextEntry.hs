module Changes.UI.GTK.TextEntry
    ( createTextEntry
    ) where

import Changes.Core
import Changes.GI
import GI.Gdk
import GI.Gtk as Gtk
import Shapes hiding (get)
import Changes.Debug

createTextEntry :: Model (WholeUpdate Text) -> GView 'Locked Widget
createTextEntry rmod = traceBracket "createTextEntry" $ do
    esrc <- newEditSource
    widget <- traceBracket "createTextEntry.new" $ gvNew Entry []
    invalidCol <- new RGBA [#red := 1, #green := 0, #blue := 0, #alpha := 1]
    let
        setValidState :: Bool -> GView 'Locked ()
        setValidState True = #overrideColor widget [StateFlagsNormal] Nothing
        setValidState False = #overrideColor widget [StateFlagsNormal] $ Just invalidCol
    changedSignal <-
        traceBracket "createTextEntry.on#changed" $
        gvOnSignal widget #changed $ do
            st <- gvLiftIO $ get widget #text
            succeeded <- gvRunResource rmod $ \asub -> pushEdit esrc $ aModelEdit asub $ pure $ MkWholeReaderEdit st
            setValidState succeeded
    traceBracket "createTextEntry.bindmodel" $ gvBindWholeModel rmod (Just esrc) $ \newtext ->
        withSignalBlocked widget changedSignal $ do
            oldtext <- get widget #text
            if oldtext == newtext
                then return ()
                else set widget [#text := newtext]
            setValidState True
    toWidget widget
