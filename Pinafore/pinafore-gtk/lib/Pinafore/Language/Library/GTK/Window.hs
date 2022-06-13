{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.GTK.Window
    ( windowStuff
    ) where

import Changes.Core
import Changes.UI.GTK
import Data.Shim
import Pinafore.Base
import Pinafore.Language.API
import Pinafore.Language.Library.GTK.Element
import Pinafore.Language.Library.GTK.MenuItem ()
import Shapes

-- LangWindow
data LangWindow = MkLangWindow
    { pwContext :: GTKContext
    , pwClose :: GView 'Unlocked ()
    , pwWindow :: UIWindow
    }

windowGroundType :: PinaforeGroundType '[] LangWindow
windowGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangWindow)|]) "Window"

instance HasPinaforeGroundType '[] LangWindow where
    pinaforeGroundType = windowGroundType

contextGroundType :: PinaforeGroundType '[] GTKContext
contextGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily GTKContext)|]) "Context"

instance HasPinaforeGroundType '[] GTKContext where
    pinaforeGroundType = contextGroundType

-- UIWindow
instance HasPinaforeType 'Negative UIWindow where
    pinaforeType = mapNegShimWit (functionToShim "pwWindow" pwWindow) pinaforeType

createLangWindow :: GTKContext -> WindowSpec -> PinaforeAction LangWindow
createLangWindow gtkc uiw = do
    (pwWindow, pwClose) <- actionLiftView $ runGView gtkc $ gvGetCloser $ gvRunLocked $ createWindow uiw
    let pwContext = gtkc
    return $ MkLangWindow {..}

uiWindowClose :: LangWindow -> View ()
uiWindowClose MkLangWindow {..} = runGView pwContext pwClose

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
            wsCloseBoxAction = gvRunUnlocked $ pwClose w
            wsTitle :: Model (ROWUpdate Text)
            wsTitle = unWModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyRef title
            wsMenuBar :: Maybe (Model (ROWUpdate MenuBar))
            wsMenuBar = Just $ unWModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyRef mbar
            in MkWindowSpec {..}

exitUI :: GTKContext -> View ()
exitUI gtkc = runGView gtkc $ gvExitUI

windowStuff :: DocTreeEntry BindDoc
windowStuff =
    docTreeEntry
        "Window"
        ""
        [ mkTypeEntry "Window" "A user interface window." $ MkBoundType windowGroundType
        , mkValEntry "openWindow" "Open a new window with this size, title and element." openWindow
        , mkValEntry "closeWindow" "Close a window." uiWindowClose
        , mkValEntry "showWindow" "Show a window." uiWindowShow
        , mkValEntry "hideWindow" "Hide a window." uiWindowHide
        , mkValEntry "exitUI" "Exit the user interface." exitUI
        ]
