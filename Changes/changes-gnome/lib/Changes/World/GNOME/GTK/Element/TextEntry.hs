module Changes.World.GNOME.GTK.Element.TextEntry
    ( createTextEntry
    ) where

import Changes.Core
import Changes.World.GNOME.GI
import GI.Gdk
import GI.Gtk as Gtk
import Shapes hiding (get)

createTextEntry :: Model (WholeUpdate Text) -> GView 'Locked Widget
createTextEntry rmod = do
    esrc <- newEditSource
    widget <- gvNew Entry []
    invalidCol <- new RGBA [#red := 1, #green := 0, #blue := 0, #alpha := 1]
    let
        setValidState :: Bool -> GView 'Locked ()
        setValidState True = #overrideColor widget [StateFlagsNormal] Nothing
        setValidState False = #overrideColor widget [StateFlagsNormal] $ Just invalidCol
    changedSignal <-
        gvOnSignal widget #changed $ do
            st <- gvLiftIO $ get widget #text
            succeeded <- gvRunResource rmod $ \asub -> pushEdit esrc $ aModelEdit asub $ pure $ MkWholeReaderEdit st
            setValidState succeeded
    gvBindWholeModel rmod (Just esrc) $ \newtext ->
        withSignalBlocked widget changedSignal $ do
            oldtext <- get widget #text
            if oldtext == newtext
                then return ()
                else set widget [#text := newtext]
            setValidState True
    toWidget widget
