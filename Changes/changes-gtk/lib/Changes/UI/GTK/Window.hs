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
import Changes.UI.GTK.MenuBar
import Changes.UI.GTK.Switch
import GI.GLib as GI hiding (String)
import GI.Gtk as GI hiding (MenuBar)
import Shapes

data WindowSpec = MkWindowSpec
    { wsPosition :: WindowPosition
    , wsSize :: (Int32, Int32)
    , wsCloseBoxAction :: GView 'Locked ()
    , wsTitle :: Model (ROWUpdate Text)
    , wsMenuBar :: Maybe (Model (ROWUpdate MenuBar))
    , wsContent :: GView 'Locked Widget
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
    content <- wsContent
    ui <-
        case wsMenuBar of
            Nothing -> return content
            Just efmbar -> do
                ag <- gvNew AccelGroup []
                #addAccelGroup window ag
                mb <-
                    createDynamic $
                    mapModel (liftReadOnlyChangeLens $ funcChangeLens $ \mbar -> createMenuBar ag mbar >>= toWidget) $
                    efmbar
                vbox <- gvNew Box [#orientation := OrientationVertical]
                #packStart vbox mb False False 0
                #packStart vbox content True True 0
                toWidget vbox
    #add window ui
    #show ui
    #showAll window
    let
        uiWindowHide = #hide window
        uiWindowShow = #show window
        uiWindowDebugDescribe = do
            w <- toWidget window
            widgetInfoText w
    return $ MkUIWindow {..}
