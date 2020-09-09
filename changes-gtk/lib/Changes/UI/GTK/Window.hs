module Changes.UI.GTK.Window
    ( WindowSpec(..)
    , UIWindow(..)
    , createWindow
    ) where

import Changes.Core
import Changes.UI.GTK.MenuBar
import Changes.UI.GTK.Switch
import Changes.UI.GTK.Useful
import GI.GLib as GI hiding (String)
import GI.Gtk as GI hiding (MenuBar)
import Shapes
import Changes.Debug.Reference

data WindowSpec = MkWindowSpec
    { wsCloseBoxAction :: View ()
    , wsTitle :: Model (ROWUpdate Text)
    , wsMenuBar :: Maybe (Model (ROWUpdate MenuBar))
    , wsContent :: Widget
    }

data UIWindow = MkUIWindow
    { uiWindowHide :: View ()
    , uiWindowShow :: View ()
    }

createWindow :: WindowSpec -> CreateView UIWindow
createWindow MkWindowSpec {..} = do
    window <-
        cvTopLevelNew Window [#windowPosition := WindowPositionCenter, #defaultWidth := 300, #defaultHeight := 400]
    cvBindReadOnlyWholeModel wsTitle $ \title -> set window [#title := title]
    _ <-
        cvOn window #deleteEvent $ \_ -> traceBracket "GTK.Window:close" $ do
            wsCloseBoxAction
            return True -- don't run existing handler that closes the window
    ui <-
        case wsMenuBar of
            Nothing -> return wsContent
            Just efmbar -> do
                ag <- cvNew AccelGroup []
                #addAccelGroup window ag
                mb <-
                    createDynamic $
                    mapModel (liftReadOnlyChangeLens $ funcChangeLens $ \mbar -> createMenuBar ag mbar >>= toWidget) $
                    efmbar
                vbox <- cvNew Box [#orientation := OrientationVertical]
                #packStart vbox mb False False 0
                #packStart vbox wsContent True True 0
                toWidget vbox
    #add window ui
    #show ui
    #showAll window
    let
        uiWindowHide = #hide window
        uiWindowShow = #show window
    return $ MkUIWindow {..}