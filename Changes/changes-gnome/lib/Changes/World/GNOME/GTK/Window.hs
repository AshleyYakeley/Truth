module Changes.World.GNOME.GTK.Window
    ( WindowSpec(..)
    , UIWindow
    , uiWindowHide
    , uiWindowShow
    , uiWindowDebugDescribe
    , createWindow
    , WindowPosition(..)
    ) where

import Changes.Core
import Changes.World.GNOME.GI
import GI.Gtk as GI
import Shapes

data WindowSpec = MkWindowSpec
    { wsPosition :: WindowPosition
    , wsSize :: (Int32, Int32)
    , wsCloseBoxAction :: GView 'Locked ()
    , wsTitle :: Model (ROWUpdate Text)
    , wsContent :: AccelGroup -> GView 'Unlocked Widget
    }

data UIWindow = MkUIWindow
    { uiWindowHide :: GView 'Locked ()
    , uiWindowShow :: GView 'Locked ()
    , uiWindowDebugDescribe :: IO Text
    }

createWindow :: WindowSpec -> GView 'Unlocked UIWindow
createWindow MkWindowSpec {..} = do
    window <-
        gvExitOnClosed $
        gvRunLocked $
        gvTopLevelNew Window [#windowPosition := wsPosition, #defaultWidth := fst wsSize, #defaultHeight := snd wsSize]
    gvBindReadOnlyWholeModel wsTitle $ \title -> gvRunLocked $ set window [#title := title]
    ag <-
        gvRunLocked $ do
            _ <-
                gvOnSignal window #deleteEvent $ \_ -> do
                    wsCloseBoxAction
                    return True -- don't run existing handler that closes the window
            ag <- gvNew AccelGroup []
            #addAccelGroup window ag
            return ag
    content <- wsContent ag
    gvRunLocked $ do
        #add window content
        #show content
        #showAll window
        let
            uiWindowHide = #hide window
            uiWindowShow = #show window
            uiWindowDebugDescribe = do
                w <- toWidget window
                widgetInfoText w
        return $ MkUIWindow {..}
