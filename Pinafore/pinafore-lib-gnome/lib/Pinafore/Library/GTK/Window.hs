{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Library.GTK.Window
    ( windowStuff
    , dialogStuff
    ) where

import Changes.Core
import Changes.World.GNOME.GTK
import Data.Shim
import GI.Gtk as GI hiding (Action)
import Pinafore.API
import Pinafore.Library.GIO
import Pinafore.Library.GTK.Context
import Pinafore.Library.GTK.Widget
import Shapes

-- LangWindow
data LangWindow = MkLangWindow
    { lwContext :: LangContext
    , lwClose :: GView 'Unlocked ()
    , lwWindow :: UIWindow
    }

windowGroundType :: QGroundType '[] LangWindow
windowGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangWindow)|]) "Window.GTK."

instance HasQGroundType '[] LangWindow where
    qGroundType = windowGroundType

-- UIWindow
instance HasQType QPolyShim 'Negative UIWindow where
    qType = mapNegShimWit (functionToShim "lwWindow" lwWindow) qType

createLangWindow :: LangContext -> WindowSpec -> View LangWindow
createLangWindow lc uiw = do
    (lwWindow, wclose) <- runGView (lcGTKContext lc) $ gvGetCloser $ createWindow uiw
    let lwContext = lc
    let lwClose = wclose
    return $ MkLangWindow {..}

uiWindowClose :: LangWindow -> View ()
uiWindowClose MkLangWindow {..} = runGView (lcGTKContext lwContext) lwClose

openWindow :: LangContext -> (Int32, Int32) -> ImmutableWholeModel Text -> LangWidget -> Action LangWindow
openWindow lc wsSize title (MkLangWidget widget) =
    actionLiftView $
    mfix $ \w ->
        liftIOWithUnlift $ \unlift ->
            unlift $
            createLangWindow lc $ let
                wsPosition = WindowPositionCenter
                wsCloseBoxAction :: GView 'Locked ()
                wsCloseBoxAction = gvRunUnlocked $ lwClose w
                wsTitle :: Model (ROWUpdate Text)
                wsTitle = unWModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableModelToReadOnlyModel title
                wsContent :: AccelGroup -> GView 'Unlocked Widget
                wsContent ag =
                    widget
                        MkWidgetContext
                            { wcUnlift = unlift
                            , wcAccelGroup = ag
                            , wcSelectNotify = mempty
                            , wcOtherContext = lcOtherContext lc
                            }
                in MkWindowSpec {..}

exitUI :: LangContext -> View ()
exitUI lc = runGView (lcGTKContext lc) $ gvExitUI

showWindow :: LangWindow -> View ()
showWindow MkLangWindow {..} = runGView (lcGTKContext lwContext) $ gvRunLocked $ uiWindowShow lwWindow

hideWindow :: LangWindow -> View ()
hideWindow MkLangWindow {..} = runGView (lcGTKContext lwContext) $ gvRunLocked $ uiWindowHide lwWindow

run :: forall a. (LangContext -> Action a) -> Action a
run call =
    actionTunnelView $ \unlift ->
        runGTKView $ \gtkc -> do
            clipboard <- runGView gtkc getClipboard
            unlift $
                call $ MkLangContext {lcGTKContext = gtkc, lcOtherContext = MkOtherContext {ocClipboard = clipboard}}

windowStuff :: LibraryStuff
windowStuff =
    headingBDS
        "Windows"
        ""
        [ typeBDS "Context" "Context for GTK" (MkSomeGroundType contextGroundType) []
        , valBDS
              "run"
              "Call the provided function with a GTK context, after which run the GTK event loop until all windows are closed." $
          run @A
        , typeBDS "Window" "A user interface window." (MkSomeGroundType windowGroundType) []
        , namespaceBDS
              "Window"
              [ valBDS "open" "Open a new window with this size, title and widget." openWindow
              , valBDS "close" "Close a window." uiWindowClose
              , valBDS "show" "Show a window." showWindow
              , valBDS "hide" "Hide a window." hideWindow
              ]
        , valBDS "exit" "Exit the user interface." exitUI
        ]

langChooseFile :: FileChooserAction -> LangContext -> (Maybe (Text, Text) -> Bool) -> Action LangFile
langChooseFile action lc test =
    actionLiftViewKnow $ fmap maybeToKnow $ runGView (lcGTKContext lc) $ gvRunLocked $ chooseFile action test

dialogStuff :: LibraryStuff
dialogStuff =
    headingBDS
        "Dialogs"
        ""
        [ valBDS "chooseExistingFile" "Run a dialog to choose an existing file." $ langChooseFile FileChooserActionOpen
        , valBDS "chooseNewFile" "Run a dialog to choose a new file." $ langChooseFile FileChooserActionSave
        ]
