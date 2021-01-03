module Changes.UI.GTK.Window
    ( WindowSpec(..)
    , UIWindow
    , uiWindowHide
    , uiWindowShow
    , uiWindowDebugDescribe
    , createWindow
    ) where

import Changes.Core
import Changes.UI.GTK.MenuBar
import Changes.UI.GTK.Switch
import Changes.UI.GTK.Useful
import GI.GLib as GI hiding (String)
import GI.Gtk as GI hiding (MenuBar)
import Shapes

data WindowSpec = MkWindowSpec
    { wsCloseBoxAction :: View ()
    , wsTitle :: Model (ROWUpdate Text)
    , wsMenuBar :: Maybe (Model (ROWUpdate MenuBar))
    , wsContent :: CreateView Widget
    }

data UIWindow = MkUIWindow
    { uiWindowHide :: View ()
    , uiWindowShow :: View ()
    , uiWindowDebugDescribe :: IO Text
    }

createWindow :: WindowSpec -> CreateView UIWindow
createWindow MkWindowSpec {..} = do
    window <-
        cvTopLevelNew Window [#windowPosition := WindowPositionCenter, #defaultWidth := 300, #defaultHeight := 400]
    cvBindReadOnlyWholeModel wsTitle $ \title -> set window [#title := title]
    _ <-
        cvOn window #deleteEvent $ \_ -> do
            wsCloseBoxAction
            return True -- don't run existing handler that closes the window
    content <- wsContent
    ui <-
        case wsMenuBar of
            Nothing -> return content
            Just efmbar -> do
                ag <- cvNew AccelGroup []
                #addAccelGroup window ag
                mb <-
                    createDynamic $
                    mapModel (liftReadOnlyChangeLens $ funcChangeLens $ \mbar -> createMenuBar ag mbar >>= toWidget) $
                    efmbar
                vbox <- cvNew Box [#orientation := OrientationVertical]
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
