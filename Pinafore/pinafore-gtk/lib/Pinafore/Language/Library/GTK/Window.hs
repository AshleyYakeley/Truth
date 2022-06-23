{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.GTK.Window
    ( windowStuff
    ) where

import Changes.Core
import Changes.UI.GTK
import Data.Shim
import Pinafore.Base
import Pinafore.Language.API
import Pinafore.Language.Library.GTK.Context
import Pinafore.Language.Library.GTK.Element
import Pinafore.Language.Library.GTK.MenuItem ()
import Shapes

-- LangWindow
data LangWindow = MkLangWindow
    { lwContext :: GTKContext
    , lwClose :: GView 'Unlocked ()
    , lwWindow :: UIWindow
    }

windowGroundType :: PinaforeGroundType '[] LangWindow
windowGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangWindow)|]) "Window"

instance HasPinaforeGroundType '[] LangWindow where
    pinaforeGroundType = windowGroundType

-- UIWindow
instance HasPinaforeType 'Negative UIWindow where
    pinaforeType = mapNegShimWit (functionToShim "lwWindow" lwWindow) pinaforeType

createLangWindow :: GTKContext -> WindowSpec -> PinaforeAction LangWindow
createLangWindow gtkc uiw = do
    (lwWindow, wclose) <- actionLiftView $ runGView gtkc $ gvGetCloser $ gvRunLocked $ createWindow uiw
    let lwContext = gtkc
    let lwClose = wclose
    return $ MkLangWindow {..}

uiWindowClose :: LangWindow -> View ()
uiWindowClose MkLangWindow {..} = runGView lwContext lwClose

openWindow ::
       (?pinafore :: PinaforeContext)
    => GTKContext
    -> (Int32, Int32)
    -> PinaforeImmutableWholeRef Text
    -> PinaforeImmutableWholeRef MenuBar
    -> LangElement
    -> PinaforeAction LangWindow
openWindow gtkc wsSize title mbar (MkLangElement wsContent) =
    mfix $ \w ->
        createLangWindow gtkc $ let
            wsPosition = WindowPositionCenter
            wsCloseBoxAction :: GView 'Locked ()
            wsCloseBoxAction = gvRunUnlocked $ lwClose w
            wsTitle :: Model (ROWUpdate Text)
            wsTitle = unWModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyRef title
            wsMenuBar :: Maybe (Model (ROWUpdate MenuBar))
            wsMenuBar = Just $ unWModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyRef mbar
            in MkWindowSpec {..}

exitUI :: GTKContext -> View ()
exitUI gtkc = runGView gtkc $ gvExitUI

showWindow :: LangWindow -> View ()
showWindow MkLangWindow {..} = runGView lwContext $ gvRunLocked $ uiWindowShow lwWindow

hideWindow :: LangWindow -> View ()
hideWindow MkLangWindow {..} = runGView lwContext $ gvRunLocked $ uiWindowHide lwWindow

run :: forall a. (GTKContext -> PinaforeAction a) -> PinaforeAction a
run call = actionTunnelView $ \unlift -> runGTKView $ \gtkc -> unlift $ call gtkc

windowStuff :: DocTreeEntry BindDoc
windowStuff =
    docTreeEntry
        "Window"
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
