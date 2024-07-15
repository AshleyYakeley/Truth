module Changes.World.GNOME.GTK.Widget.TextEntry where

import Changes.Core
import Changes.World.GNOME.GI
import GI.Gdk
import GI.Gtk as Gtk
import Shapes hiding (get)

attachEntryText :: Entry -> Model (WholeUpdate Text) -> GView 'Unlocked ()
attachEntryText entry rmod = do
    esrc <- gvNewEditSource
    gvRunLockedThen $ do
        invalidCol <- new RGBA [#red := 1, #green := 0, #blue := 0, #alpha := 1]
        let
            setValidState :: Bool -> GView 'Locked ()
            setValidState True = #overrideColor entry [StateFlagsNormal] Nothing
            setValidState False = #overrideColor entry [StateFlagsNormal] $ Just invalidCol
        changedSignal <-
            gvOnSignal entry #changed $ do
                st <- gvLiftIO $ get entry #text
                succeeded <- gvRunUnlocked $ gvSetWholeModel rmod esrc st
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

createEntry :: GView 'Locked (Entry, Widget)
createEntry = gvNewWidget Entry []

createTextEntry :: Model (WholeUpdate Text) -> GView 'Unlocked Widget
createTextEntry rmod = do
    (entry, widget) <- gvRunLocked $ gvNewWidget Entry []
    attachEntryText entry rmod
    return widget
