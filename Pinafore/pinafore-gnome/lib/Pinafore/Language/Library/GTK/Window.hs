{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.GTK.Window
    ( windowStuff
    , dialogStuff
    ) where

import Changes.Core
import Changes.World.GNOME.GTK
import Data.Shim
import GI.Gtk as GI hiding (Action)
import Pinafore.Base
import Pinafore.Language.API
import Pinafore.Language.Library.GIO
import Pinafore.Language.Library.GTK.Context
import Pinafore.Language.Library.GTK.Element
import Shapes

-- LangWindow
data LangWindow = MkLangWindow
    { lwContext :: LangContext
    , lwClose :: GView 'Locked ()
    , lwWindow :: UIWindow
    }

windowGroundType :: QGroundType '[] LangWindow
windowGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangWindow)|]) "Window.GTK."

instance HasQGroundType '[] LangWindow where
    qGroundType = windowGroundType

-- UIWindow
instance HasQType 'Negative UIWindow where
    qType = mapNegShimWit (functionToShim "lwWindow" lwWindow) qType

createLangWindow :: LangContext -> WindowSpec -> View LangWindow
createLangWindow lc uiw = do
    (lwWindow, wclose) <- runGView (lcGTKContext lc) $ gvRunLocked $ gvGetCloser $ createWindow uiw
    let lwContext = lc
    let lwClose = wclose
    return $ MkLangWindow {..}

uiWindowClose :: LangWindow -> View ()
uiWindowClose MkLangWindow {..} = runGView (lcGTKContext lwContext) $ gvRunLocked lwClose

openWindow :: LangContext -> (Int32, Int32) -> ImmutableWholeModel Text -> LangElement -> Action LangWindow
openWindow lc wsSize title (MkLangElement element) =
    actionLiftView $
    mfix $ \w ->
        liftIOWithUnlift $ \unlift ->
            unlift $
            createLangWindow lc $ let
                wsPosition = WindowPositionCenter
                wsCloseBoxAction :: GView 'Locked ()
                wsCloseBoxAction = lwClose w
                wsTitle :: Model (ROWUpdate Text)
                wsTitle = unWModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableModelToReadOnlyModel title
                wsContent :: AccelGroup -> GView 'Locked Widget
                wsContent ag =
                    element
                        MkElementContext
                            { ecUnlift = unlift
                            , ecAccelGroup = ag
                            , ecSelectNotify = mempty
                            , ecOtherContext = lcOtherContext lc
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
            clipboard <- runGView gtkc $ gvRunLocked getClipboard
            unlift $
                call $ MkLangContext {lcGTKContext = gtkc, lcOtherContext = MkOtherContext {ocClipboard = clipboard}}

windowStuff :: BindDocTree ()
windowStuff =
    headingBDT
        "Windows"
        ""
        [ typeBDT "Context" "Context for GTK" (MkSomeGroundType contextGroundType) []
        , valBDT
              "run"
              "Call the provided function with a GTK context, after which run the GTK event loop until all windows are closed." $
          run @A
        , typeBDT "Window" "A user interface window." (MkSomeGroundType windowGroundType) []
        , valBDT "openWindow" "Open a new window with this size, title and element." openWindow
        , valBDT "closeWindow" "Close a window." uiWindowClose
        , valBDT "showWindow" "Show a window." showWindow
        , valBDT "hideWindow" "Hide a window." hideWindow
        , valBDT "exit" "Exit the user interface." exitUI
        ]

langChooseFile :: FileChooserAction -> LangContext -> (Maybe (Text, Text) -> Bool) -> Action File
langChooseFile action lc test =
    actionLiftViewKnow $ fmap maybeToKnow $ runGView (lcGTKContext lc) $ gvRunLocked $ chooseFile action test

dialogStuff :: BindDocTree ()
dialogStuff =
    headingBDT
        "Dialogs"
        ""
        [ valBDT "chooseExistingFile" "Run a dialog to choose an existing file." $ langChooseFile FileChooserActionOpen
        , valBDT "chooseNewFile" "Run a dialog to choose a new file." $ langChooseFile FileChooserActionSave
        ]
