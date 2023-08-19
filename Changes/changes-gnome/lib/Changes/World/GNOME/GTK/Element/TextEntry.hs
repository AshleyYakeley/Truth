module Changes.World.GNOME.GTK.Element.TextEntry
    ( createTextEntry
    ) where

import Changes.Core
import Changes.World.GNOME.GI
import GI.Gdk
import GI.Gtk as Gtk
import Shapes hiding (get)

createTextEntry :: Model (WholeUpdate Text) -> GView 'Unlocked Widget
createTextEntry rmod = do
    esrc <- gvNewEditSource
    gvRunLockedThen $ do
        (entry, widget) <- gvNewWidget Entry []
        invalidCol <- new RGBA [#red := 1, #green := 0, #blue := 0, #alpha := 1]
        let
            setValidState :: Bool -> GView 'Locked ()
            setValidState True = #overrideColor entry [StateFlagsNormal] Nothing
            setValidState False = #overrideColor entry [StateFlagsNormal] $ Just invalidCol
        changedSignal <-
            gvOnSignal entry #changed $ do
                st <- gvLiftIO $ get entry #text
                succeeded <-
                    gvRunUnlocked $
                    gvLiftView $
                    viewRunResource rmod $ \asub -> pushEdit esrc $ aModelEdit asub $ pure $ MkWholeReaderEdit st
                setValidState succeeded
        return $ do
            gvBindWholeModel rmod (Just esrc) $ \newtext ->
                gvRunLocked $
                withSignalBlocked entry changedSignal $ do
                    oldtext <- get entry #text
                    if oldtext == newtext
                        then return ()
                        else set entry [#text := newtext]
                    setValidState True
            return widget
