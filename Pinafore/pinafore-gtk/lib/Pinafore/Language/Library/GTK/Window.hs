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
    { pwClose :: View ()
    , pwWindow :: UIWindow
    }

windowGroundType :: PinaforeGroundType '[] LangWindow
windowGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (HetEqual LangWindow)|]) "Window"

instance HasPinaforeGroundType '[] LangWindow where
    pinaforeGroundType = windowGroundType

-- UIWindow
instance HasPinaforeType 'Negative UIWindow where
    pinaforeType = mapNegShimWit (functionToShim "pwWindow" pwWindow) pinaforeType

createLangWindow :: WindowSpec -> PinaforeAction LangWindow
createLangWindow uiw = do
    MkWMFunction exitOnClose <- pinaforeGetExitOnClose
    (pwWindow, close) <- pinaforeEarlyCloser $ createViewPinaforeAction $ exitOnClose $ createWindow uiw
    let pwClose = liftIO close
    return $ MkLangWindow {..}

openWindow ::
       (?pinafore :: PinaforeContext)
    => (Int32, Int32)
    -> PinaforeImmutableWholeRef Text
    -> PinaforeImmutableWholeRef MenuBar
    -> LangElement
    -> PinaforeAction LangWindow
openWindow wsSize title mbar (MkLangElement wsContent) =
    mfix $ \w ->
        createLangWindow $ let
            wsPosition = WindowPositionCenter
            wsCloseBoxAction :: View ()
            wsCloseBoxAction = pwClose w
            wsTitle :: Model (ROWUpdate Text)
            wsTitle = unWModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyRef title
            wsMenuBar :: Maybe (Model (ROWUpdate MenuBar))
            wsMenuBar = Just $ unWModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyRef mbar
            in MkWindowSpec {..}

windowStuff :: DocTreeEntry BindDoc
windowStuff =
    docTreeEntry
        "Window"
        ""
        [ mkTypeEntry "Window" "A user interface window." $ MkBoundType windowGroundType
        , mkValEntry "openWindow" "Open a new window with this size, title and element." openWindow
        , mkValEntry "closeWindow" "Close a window." pwClose
        , mkValEntry "showWindow" "Show a window." uiWindowShow
        , mkValEntry "hideWindow" "Hide a window." uiWindowHide
        , mkValEntry "exitUI" "Exit the user interface." pinaforeExit
        ]
