module Changes.World.GNOME.GTK.Window
    ( WindowSpec (..)
    , createWindow
    )
where

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

data WindowSpec = MkWindowSpec
    { wsSize :: (Int32, Int32)
    , wsCloseBoxAction :: GView 'Locked ()
    , wsTitle :: Model (ROWUpdate Text)
    , wsContent :: GView 'Unlocked GI.Widget
    }

createWindow :: WindowSpec -> GView 'Unlocked GI.Window
createWindow MkWindowSpec{..} = do
    gvExitOnClosed
    window <-
        gvRunLocked
            $ gvNewWindow GI.Window [#defaultWidth GI.:= fst wsSize, #defaultHeight GI.:= snd wsSize]
    gvBindReadOnlyWholeModel wsTitle $ \title -> gvRunLocked $ GI.set window [#title GI.:= title]
    gvRunLocked $ do
        _ <-
            gvOnSignal True window #closeRequest $ do
                wsCloseBoxAction
                return True -- don't run existing handler that closes the window
        return ()
    content <- wsContent
    gvRunLocked $ do
        #setChild window $ Just content
        #present window
        return window
