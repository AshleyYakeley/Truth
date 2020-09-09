module Changes.UI.GTK.TextEntry
    ( createTextEntry
    ) where

import Changes.Core
import Changes.UI.GTK.Useful
import GI.Gdk
import GI.Gtk as Gtk
import Shapes hiding (get)

createTextEntry :: Model (WholeUpdate Text) -> CreateView Widget
createTextEntry rmod = do
    esrc <- newEditSource
    widget <- cvNew Entry []
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
        viewRunResource rmod $ \asub -> do
            st <- get widget #text
            succeeded <- pushEdit esrc $ aModelEdit asub $ pure $ MkWholeReaderEdit st
            setValidState succeeded
    cvBindWholeModel rmod (Just esrc) $ \newtext ->
        withSignalBlocked widget changedSignal $ do
            oldtext <- get widget #text
            if oldtext == newtext
                then return ()
                else set widget [#text := newtext]
            setValidState True
    toWidget widget
