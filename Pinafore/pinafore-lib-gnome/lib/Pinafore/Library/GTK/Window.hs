{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Library.GTK.Window
    ( windowStuff
    , dialogStuff
    )
where

import Changes.Core
import Changes.World.GNOME.GTK
import Data.Shim
import GI.Gtk (FileChooserAction (..))
import GI.Gtk qualified as GI
import Pinafore.API
import Pinafore.Library.Media
import Shapes

import Pinafore.Library.GIO
import Pinafore.Library.GTK.Context
import Pinafore.Library.GTK.Widget

-- LangWindow
data LangWindow = MkLangWindow
    { lwContext :: LangContext
    , lwClose :: GSemiview 'Unlocked ()
    , lwWindow :: Window
    }

windowGroundType :: QGroundType '[] LangWindow
windowGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangWindow)|]) "Window.GTK."

instance HasQGroundType '[] LangWindow where
    qGroundType = windowGroundType

-- Window
instance HasQType QPolyShim 'Negative Window where
    qType = mapNegShimWit (functionToShim "lwWindow" lwWindow) qType

createLangWindow :: LangContext -> WindowSpec -> View LangWindow
createLangWindow lc uiw = do
    (lwWindow, wclose) <- runLangContext lc $ gvGetCloser $ createWindow uiw
    let lwContext = lc
    let lwClose = wclose
    return $ MkLangWindow{..}

uiWindowClose :: LangWindow -> Semiview ()
uiWindowClose MkLangWindow{..} = runGSemiview (lcGTKContext lwContext) lwClose

openWindow :: LangContext -> (Natural, Natural) -> ImmutableWholeModel Text -> LangWidget -> Action LangWindow
openWindow lc (w, h) title (MkLangWidget widget) =
    actionLiftView
        $ mfix
        $ \window ->
            liftIOWithUnlift $ \unlift ->
                unlift
                    $ createLangWindow lc
                    $ let
                        wsSize :: (Int32, Int32)
                        wsSize = (fromIntegral w, fromIntegral h)
                        wsCloseBoxAction :: GView 'Locked ()
                        wsCloseBoxAction = lift $ gsvRunUnlocked $ lwClose window
                        wsTitle :: Model (ROWUpdate Text)
                        wsTitle = unWModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableModelToReadOnlyModel title
                        wsContent :: GView 'Unlocked Widget
                        wsContent =
                            widget
                                MkWidgetContext
                                    { wcUnlift = unlift
                                    , wcSelectNotify = mempty
                                    , wcOtherContext = lcOtherContext lc
                                    }
                        in MkWindowSpec{..}

exitUI :: LangContext -> ActionException -> View ()
exitUI lc aex = runLangContext lc $ gvExitUI $ case aex of
    ExActionException ex -> FailureResult ex
    StopActionException -> SuccessResult ()

showWindow :: LangWindow -> View ()
showWindow MkLangWindow{..} = runLangContext lwContext $ gvRunLocked $ #present lwWindow

hideWindow :: LangWindow -> View ()
hideWindow MkLangWindow{..} = runLangContext lwContext $ gvRunLocked $ #setVisible lwWindow False

run :: forall a. (LangContext -> Action a) -> Action a
run call =
    actionTunnelView $ \unlift ->
        runGTK $ \gtkc -> do
            clipboard <- runGView gtkc getTheClipboardModel
            unlift
                $ call
                $ MkLangContext{lcGTKContext = gtkc, lcOtherContext = MkOtherContext{ocClipboard = clipboard}}

styleSheetParams :: ListType QDocSignature '[Word32]
styleSheetParams =
    ConsListType (mkValueDocSignature "priority" "CSS priority" $ Just $ fromIntegral GI.STYLE_PROVIDER_PRIORITY_APPLICATION)
        $ NilListType

styleSheet :: ListProduct '[Word32] -> LangContext -> ImmutableWholeModel CSSText -> View ()
styleSheet (priority, ()) lc cssmodel =
    runLangContext lc
        $ bindCSS priority (unWModel $ immutableWholeModelValue mempty $ fmap unCSSText cssmodel)

windowStuff :: LibraryStuff
windowStuff =
    headingBDS
        "Windows"
        ""
        [ typeBDS "Context" "Context for GTK" (MkSomeGroundType contextGroundType) []
        , valBDS
            "run"
            "Run GTK with the provided function, then wait until all windows are closed or `exit` is called."
            $ run @A
        , valBDS "exit" "Exit the user interface." exitUI
        , typeBDS "Window" "A user interface window." (MkSomeGroundType windowGroundType) []
        , namespaceBDS
            "Window"
            [ valBDS "open" "Open a new window with this size, title and widget." openWindow
            , valBDS "close" "Close a window." uiWindowClose
            , valBDS "show" "Show a window." showWindow
            , valBDS "hide" "Hide a window." hideWindow
            ]
        , recordValueBDS
            "styleSheet"
            "Add a CSS style-sheet for GTK (for the duration of the lifecycle). \
            \See the GTK CSS [overview](https://docs.gtk.org/gtk4/css-overview.html) and [properties](https://docs.gtk.org/gtk4/css-properties.html) for how this works."
            styleSheetParams
            styleSheet
        ]

langChooseFile :: FileChooserAction -> LangContext -> LangWindow -> Maybe [(Text, Maybe Text)] -> Action (LangStoppableTask LangFile)
langChooseFile action lc lw test =
    fmap (MkLangStoppableTask . hoistStoppableTask (actionLiftView . runLangContext lc))
        $ actionLiftView
        $ runLangContext lc
        $ gvRunLocked
        $ chooseFile action (lwWindow lw) test

dialogStuff :: LibraryStuff
dialogStuff =
    headingBDS
        "Dialogs"
        ""
        [ valBDS "chooseExistingFile" "Create a dialog to choose an existing file." $ langChooseFile FileChooserActionOpen
        , valBDS "chooseNewFile" "Create a dialog to choose a new file." $ langChooseFile FileChooserActionSave
        ]
