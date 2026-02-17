module Changes.World.GNOME.GTK.Widget.TextEntry where

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

attachEntryText :: GI.Entry -> Model (WholeUpdate Text) -> GView 'Unlocked ()
attachEntryText entry rmod = do
    esrc <- gvNewEditSource
    gvRunLockedThen $ do
        invalidCol <- GI.new GI.RGBA [#red GI.:= 1, #green GI.:= 0, #blue GI.:= 0, #alpha GI.:= 1]
        let
            setValidState :: Bool -> GView 'Locked ()
            setValidState True = #overrideColor entry [GI.StateFlagsNormal] Nothing
            setValidState False = #overrideColor entry [GI.StateFlagsNormal] $ Just invalidCol
        changedSignal <-
            gvOnSignal entry #changed $ do
                st <- gvLiftIO $ get entry #text
                succeeded <- gvRunUnlocked $ gvSetWholeModel rmod esrc st
                setValidState succeeded
        return $ do
            gvBindWholeModel rmod (Just esrc) $ \newtext ->
                gvRunLocked
                    $ withSignalBlocked entry changedSignal
                    $ do
                        oldtext <- get entry #text
                        if oldtext == newtext
                            then return ()
                            else GI.set entry [#text GI.:= newtext]
                        setValidState True

createEntry :: GView 'Locked (GI.Entry, GI.Widget)
createEntry = gvNewWidget GI.Entry []

createTextEntry :: Model (WholeUpdate Text) -> GView 'Unlocked GI.Widget
createTextEntry rmod = do
    (entry, widget) <- gvRunLocked $ gvNewWidget GI.Entry []
    attachEntryText entry rmod
    return widget
