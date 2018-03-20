module Truth.UI.GTK.Window
    ( WindowButtons(..)
    , SomeUIWindow(..)
    , truthMain
    , getMaybeView
    ) where

import GI.GLib hiding (String)
import GI.Gtk
import Shapes
import System.Environment
import Truth.Core
import Truth.UI.GTK.Button
import Truth.UI.GTK.CSS
import Truth.UI.GTK.CheckButton
import Truth.UI.GTK.Drag
import Truth.UI.GTK.Entry
import Truth.UI.GTK.GView
import Truth.UI.GTK.Icon
import Truth.UI.GTK.Label
import Truth.UI.GTK.Layout
import Truth.UI.GTK.Maybe
import Truth.UI.GTK.Option
import Truth.UI.GTK.Pages
import Truth.UI.GTK.Switch
import Truth.UI.GTK.Table
import Truth.UI.GTK.Text
import Truth.UI.GTK.Useful

lastResortView :: UISpec edit -> GCreateView edit
lastResortView spec = do
    w <- liftIO $ labelNew $ Just $ "missing viewer for " <> fromString (show spec)
    toWidget w

nullGetView :: GetGView
nullGetView =
    MkGetView $ \_ uispec -> do
        MkUINull <- isUISpec uispec
        return $ do
            w <- new DrawingArea []
            toWidget w

allGetView :: GetGView
allGetView =
    mconcat
        [ nullGetView
        , lensGetView
        , cssGetView
        , buttonGetView
        , iconGetView
        , labelGetView
        , checkButtonGetView
        , optionGetView
        , textEntryGetView
        , textAreaGetView
        , tableGetView
        , oneGetView
        , switchGetView
        , layoutGetView
        , pagesGetView
        , dragGetView
        ]

getMaybeView :: UISpec edit -> Maybe (GCreateView edit)
getMaybeView = getUIView allGetView getTheView

getTheView :: UISpec edit -> GCreateView edit
getTheView spec =
    case getMaybeView spec of
        Just view -> view
        Nothing -> lastResortView spec

class WindowButtons actions where
    addButtons :: Box -> actions -> LifeCycle ()

instance WindowButtons () where
    addButtons _ () = return ()

instance (WindowButtons a, WindowButtons b) => WindowButtons (a, b) where
    addButtons vbox (a, b) = do
        addButtons vbox a
        addButtons vbox b

instance WindowButtons SaveActions where
    addButtons vbox (MkSaveActions saveactions) = do
        hbox <- new Box [#orientation := OrientationHorizontal]
        saveButton <-
            makeButton "Save" $ do
                mactions <- saveactions
                _ <-
                    case mactions of
                        Just (action, _) -> action
                        _ -> return False
                return ()
        revertButton <-
            makeButton "Revert" $ do
                mactions <- saveactions
                _ <-
                    case mactions of
                        Just (_, action) -> action
                        _ -> return False
                return ()
        #packStart hbox saveButton False False 0
        #packStart hbox revertButton False False 0
        #packStart vbox hbox False False 0

instance WindowButtons UndoActions where
    addButtons vbox MkUndoActions {..} = do
        hbox <- new Box [#orientation := OrientationHorizontal]
        undoButton <- makeButton "Undo" uaUndo
        redoButton <- makeButton "Redo" uaRedo
        #packStart hbox undoButton False False 0
        #packStart hbox redoButton False False 0
        #packStart vbox hbox False False 0

data SomeUIWindow =
    forall actions. WindowButtons actions =>
                    MkSomeUIWindow (UserInterface UIWindow actions)

attachMenuItem :: IsMenuShell menushell => menushell -> Text -> IO MenuItem
attachMenuItem menu name = do
    item <- menuItemNewWithLabel name
    menuShellAppend menu item
    return item

attachSubmenu :: MenuItem -> IO Menu
attachSubmenu item = do
    menu <- menuNew
    menuItemSetSubmenu item $ Just menu
    return menu

menuItemAction :: MenuItem -> IO () -> IO ()
menuItemAction item action = do
    _ <- on item #activate action
    return ()

createWindowAndChild ::
       WindowButtons actions => UIWindow edit -> IO () -> IO Bool -> CreateView edit (actions -> LifeCycle ())
createWindowAndChild MkUIWindow {..} openSelection closeRequest = do
    window <- lcNewDestroy Window [#windowPosition := WindowPositionCenter, #defaultWidth := 300, #defaultHeight := 400]
    cvBindEditFunction uiTitle $ \title -> set window [#title := title]
    content <- getTheView uiContent
    _ <- on window #deleteEvent $ \_ -> liftIO closeRequest
    menubar <- menuBarNew
    fileMI <- liftIO $ attachMenuItem menubar "File"
    fileMenu <- liftIO $ attachSubmenu fileMI
    closeMI <- liftIO $ attachMenuItem fileMenu "Close"
    liftIO $
        menuItemAction closeMI $ do
            ok <- closeRequest
            if ok
                then return ()
                else widgetDestroy window
    return $ \actions -> do
        box <- new Box [#orientation := OrientationVertical]
        #packStart box menubar False False 0
        addButtons box actions
        selectionButton <- makeButton "Selection" $ liftIO openSelection
            -- this is only correct if content has native scroll support, such as TextView
        sw <- new ScrolledWindow []
        scrollable <- liftIO $ isScrollable content
        if scrollable
            then #add sw content
            else do
                viewport <- new Viewport []
                #add viewport content
                #add sw viewport
        #packStart box selectionButton False False 0
        #packStart box sw True True 0
        #add window box
        #show content
        #showAll window

makeViewWindow :: WindowButtons actions => ProgramContext -> IO () -> UserInterface UIWindow actions -> IO ()
makeViewWindow pc tellclose MkUserInterface {..} = do
    rec
        ((), srCloser) <-
            runLifeCycle $ do
                rec
                    MkViewSubscription {..} <-
                        subscribeView
                            (createWindowAndChild userinterfaceSpecifier openSelection closeRequest)
                            userinterfaceSubscriber
                            openSelection
                    srWidget srAction
                    let
                        openSelection :: IO ()
                        openSelection = do
                            msel <- srGetSelection
                            case msel of
                                Just window -> makeWindowCountRef pc $ MkUserInterface userinterfaceSubscriber window
                                Nothing -> return ()
                        closeRequest :: IO Bool
                        closeRequest = do
                            srCloser
                            tellclose
                            return True -- don't run existing handler that closes the window
                return ()
    return ()

data ProgramContext = MkProgramContext
    { pcMainLoop :: MainLoop
    , pcWindowCount :: MVar Int
    }

makeWindowCountRef ::
       forall actions. WindowButtons actions
    => ProgramContext
    -> UserInterface UIWindow actions
    -> IO ()
makeWindowCountRef pc@MkProgramContext {..} ui = let
    closer =
        mvarRun pcWindowCount $ do
            i <- Shapes.get
            Shapes.put $ i - 1
            if i == 1
                then #quit pcMainLoop
                else return ()
    in mvarRun pcWindowCount $ do
           lift $ makeViewWindow pc closer ui
           i <- Shapes.get
           Shapes.put $ i + 1

truthMain :: ([String] -> IO [SomeUIWindow]) -> IO ()
truthMain getWindows = do
    args <- getArgs
    _ <- GI.Gtk.init Nothing
    pcMainLoop <- mainLoopNew Nothing False
    wms <- getWindows args
    -- _ <- timeoutAddFull (yield >> return True) priorityDefaultIdle 50
    pcWindowCount <- newMVar 0
    for_ wms $ \(MkSomeUIWindow uiw) -> makeWindowCountRef MkProgramContext {..} uiw
    c <- mvarRun pcWindowCount $ Shapes.get
    if c == 0
        then return ()
        else #run pcMainLoop
