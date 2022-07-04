{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.GTK.Window
    ( windowStuff
    , dialogStuff
    ) where

import Changes.Core
import Changes.World.GNOME.GTK
import Data.Shim
import GI.Gtk as GI
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

windowGroundType :: PinaforeGroundType '[] LangWindow
windowGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangWindow)|]) "Window"

instance HasPinaforeGroundType '[] LangWindow where
    pinaforeGroundType = windowGroundType

-- UIWindow
instance HasPinaforeType 'Negative UIWindow where
    pinaforeType = mapNegShimWit (functionToShim "lwWindow" lwWindow) pinaforeType

createLangWindow :: LangContext -> WindowSpec -> View LangWindow
createLangWindow lc uiw = do
    (lwWindow, wclose) <- runGView (lcGTKContext lc) $ gvRunLocked $ gvGetCloser $ createWindow uiw
    let lwContext = lc
    let lwClose = wclose
    return $ MkLangWindow {..}

uiWindowClose :: LangWindow -> View ()
uiWindowClose MkLangWindow {..} = runGView (lcGTKContext lwContext) $ gvRunLocked lwClose

openWindow ::
       (?pinafore :: PinaforeContext)
    => LangContext
    -> (Int32, Int32)
    -> PinaforeImmutableWholeRef Text
    -> LangElement
    -> PinaforeAction LangWindow
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
                wsTitle = unWModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyRef title
                wsContent :: AccelGroup -> GView 'Locked Widget
                wsContent ag =
                    element MkElementContext {ecUnlift = unlift, ecAccelGroup = ag, ecOtherContext = lcOtherContext lc}
                in MkWindowSpec {..}

exitUI :: LangContext -> View ()
exitUI lc = runGView (lcGTKContext lc) $ gvExitUI

showWindow :: LangWindow -> View ()
showWindow MkLangWindow {..} = runGView (lcGTKContext lwContext) $ gvRunLocked $ uiWindowShow lwWindow

hideWindow :: LangWindow -> View ()
hideWindow MkLangWindow {..} = runGView (lcGTKContext lwContext) $ gvRunLocked $ uiWindowHide lwWindow

run :: forall a. (LangContext -> PinaforeAction a) -> PinaforeAction a
run call =
    actionTunnelView $ \unlift ->
        runGTKView $ \gtkc -> do
            clipboard <- runGView gtkc $ gvRunLocked getClipboard
            unlift $
                call $ MkLangContext {lcGTKContext = gtkc, lcOtherContext = MkOtherContext {ocClipboard = clipboard}}

windowStuff :: DocTreeEntry BindDoc
windowStuff =
    docTreeEntry
        "Windows"
        ""
        [ mkTypeEntry "Context" "Context for GTK" $ MkBoundType contextGroundType
        , mkValEntry
              "run"
              "Call the provided function with a GTK context, after which run the GTK event loop until all windows are closed." $
          run @A
        , mkTypeEntry "Window" "A user interface window." $ MkBoundType windowGroundType
        , mkValEntry "openWindow" "Open a new window with this size, title and element." openWindow
        , mkValEntry "closeWindow" "Close a window." uiWindowClose
        , mkValEntry "showWindow" "Show a window." showWindow
        , mkValEntry "hideWindow" "Hide a window." hideWindow
        , mkValEntry "exit" "Exit the user interface." exitUI
        ]

langChooseFile :: FileChooserAction -> LangContext -> (Maybe (Text, Text) -> Bool) -> PinaforeAction File
langChooseFile action lc test =
    actionLiftViewKnow $ fmap maybeToKnow $ runGView (lcGTKContext lc) $ gvRunLocked $ chooseFile action test

dialogStuff :: DocTreeEntry BindDoc
dialogStuff =
    docTreeEntry
        "Dialogs"
        ""
        [ mkValEntry "chooseExistingFile" "Run a dialog to choose an existing file." $
          langChooseFile FileChooserActionOpen
        , mkValEntry "chooseNewFile" "Run a dialog to choose a new file." $ langChooseFile FileChooserActionSave
        ]
