module Changes.UI.GTK.Window
    ( WindowSpec(..)
    , UIWindow
    , uiWindowHide
    , uiWindowShow
    , uiWindowDebugDescribe
    , createWindow
    , WindowPosition(..)
    ) where

import Changes.Core
import Changes.GI

--import GI.GLib as GI hiding (String)
import GI.Gtk as GI
import Shapes

data WindowSpec = MkWindowSpec
    { wsPosition :: WindowPosition
    , wsSize :: (Int32, Int32)
    , wsCloseBoxAction :: GView 'Locked ()
    , wsTitle :: Model (ROWUpdate Text)
    , wsContent :: AccelGroup -> GView 'Locked Widget
    }

data UIWindow = MkUIWindow
    { uiWindowHide :: GView 'Locked ()
    , uiWindowShow :: GView 'Locked ()
    , uiWindowDebugDescribe :: IO Text
    }

createWindow :: WindowSpec -> GView 'Locked UIWindow
createWindow MkWindowSpec {..} = do
    window <-
        gvExitOnClosed $
        gvTopLevelNew Window [#windowPosition := wsPosition, #defaultWidth := fst wsSize, #defaultHeight := snd wsSize]
    gvBindReadOnlyWholeModel wsTitle $ \title -> gvLiftIO $ set window [#title := title]
    _ <-
        gvOnSignal window #deleteEvent $ \_ -> do
            wsCloseBoxAction
            return True -- don't run existing handler that closes the window
    ag <- gvNew AccelGroup []
    #addAccelGroup window ag
    content <- wsContent ag
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
