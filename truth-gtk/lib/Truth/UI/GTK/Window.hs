{-# LANGUAGE ViewPatterns, FlexibleContexts #-}

module Truth.UI.GTK.Window where

import Data.IORef
import Graphics.UI.Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.CSS
import Truth.UI.GTK.CheckButton
import Truth.UI.GTK.Drag
import Truth.UI.GTK.Entry
import Truth.UI.GTK.GView
import Truth.UI.GTK.Icon
import Truth.UI.GTK.Labelled
import Truth.UI.GTK.Maybe
import Truth.UI.GTK.Option
import Truth.UI.GTK.Pages
import Truth.UI.GTK.Switch
import Truth.UI.GTK.Table
import Truth.UI.GTK.Text
import Truth.UI.GTK.Tuple

lastResortView :: UISpec edit -> GCreateView edit
lastResortView spec = do
    w <- liftIO $ labelNew $ Just $ "missing viewer for " ++ show spec
    return $ toWidget w

allGetView :: GetGView
allGetView =
    mconcat
        [ lensGetView
        , cssGetView
        , iconGetView
        , labelledGetView
        , checkButtonGetView
        , optionGetView
        , textEntryGetView
        , textAreaGetView
        , tableGetView
        , oneGetView
        , switchGetView
        , verticalLayoutGetView
        , pagesGetView
        , dragGetView
        ]

getTheView :: Edit edit => UISpec edit -> GCreateView edit
getTheView spec =
    case getUIView allGetView getTheView spec of
        Just view -> view
        Nothing -> lastResortView spec

class WindowButtons actions where
    addButtons :: VBox -> actions -> IO ()

instance WindowButtons () where
    addButtons _ () = return ()

instance (WindowButtons a, WindowButtons b) => WindowButtons (a, b) where
    addButtons vbox (a, b) = do
        addButtons vbox a
        addButtons vbox b

instance WindowButtons SaveActions where
    addButtons vbox (MkSaveActions saveactions) = do
        hbox <- hBoxNew False 0
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
        boxPackStart hbox saveButton PackNatural 0
        boxPackStart hbox revertButton PackNatural 0
        boxPackStart vbox hbox PackNatural 0

instance WindowButtons UndoActions where
    addButtons vbox MkUndoActions {..} = do
        hbox <- hBoxNew False 0
        undoButton <- makeButton "Undo" uaUndo
        redoButton <- makeButton "Redo" uaRedo
        boxPackStart hbox undoButton PackNatural 0
        boxPackStart hbox redoButton PackNatural 0
        boxPackStart vbox hbox PackNatural 0

data SomeUIWindow =
    forall actions. WindowButtons actions =>
                    MkSomeUIWindow (UIWindow actions)

attachMenuItem :: MenuShellClass menushell => menushell -> String -> IO MenuItem
attachMenuItem menu name = do
    item <- menuItemNewWithLabel name
    menuShellAppend menu item
    return item

attachSubmenu :: MenuItem -> IO Menu
attachSubmenu item = do
    menu <- menuNew
    menuItemSetSubmenu item menu
    return menu

menuItemAction :: MenuItem -> IO () -> IO ()
menuItemAction item action = do
    _ <- on item menuItemActivated action
    return ()

makeViewWindow ::
       (Edit edit, WindowButtons actions)
    => GCreateView edit
    -> IORef Int
    -> IO ()
    -> String
    -> Subscriber edit actions
    -> IO ()
makeViewWindow view windowCount tellclose title sub = do
    rec
        MkViewSubscription {..} <- subscribeView view sub openSelection
        let
            openSelection :: IO ()
            openSelection = do
                msel <- srGetSelection
                case msel of
                    Just (aspname, uiwSpec) -> let
                        uiwTitle = aspname ++ " of " ++ title
                        uiwSubscriber = sub
                        in makeWindowCountRef windowCount MkUIWindow {..}
                    Nothing -> return ()
    window <- windowNew
    set window [windowTitle := title]
    windowSetPosition window WinPosCenter
    windowSetDefaultSize window 300 400
    let
        closeRequest :: IO Bool
        closeRequest = do
            srCloser
            tellclose
            return False -- run existing handler that closes the window
    _ <- on window deleteEvent $ liftIO closeRequest
    menubar <- menuBarNew
    fileMI <- attachMenuItem menubar "File"
    fileMenu <- attachSubmenu fileMI
    closeMI <- attachMenuItem fileMenu "Close"
    menuItemAction closeMI $ do
        ok <- closeRequest
        if ok
            then return ()
            else widgetDestroy window
    box <- vBoxNew False 0
    boxPackStart box menubar PackNatural 0
    addButtons box srAction
    selectionButton <- makeButton "Selection" openSelection
        -- this is only correct if srWidget has native scroll support, such as TextView
    sw <- scrolledWindowNew Nothing Nothing
    if any (isA srWidget) [gTypeViewport, gTypeTextView]
        then set sw [containerChild := srWidget]
        else do
            hadj <- adjustmentNew 0 0 0 0 0 0
            vadj <- adjustmentNew 0 0 0 0 0 0
            viewport <- viewportNew hadj vadj
            containerAdd viewport srWidget
            set sw [containerChild := viewport]
    boxPackStart box selectionButton PackNatural 0
    boxPackStart box sw PackGrow 0
    set window [containerChild := box]
    widgetShow srWidget
    widgetShowAll window

makeViewWindowCountRef ::
       (Edit edit, WindowButtons actions) => GCreateView edit -> IORef Int -> String -> Subscriber edit actions -> IO ()
makeViewWindowCountRef view windowCount title sub = do
    makeViewWindow
        view
        windowCount
        (do
             i <- readIORef windowCount
             writeIORef windowCount (i - 1)
             if i == 1
                 then mainQuit
                 else return ())
        title
        sub
    i <- readIORef windowCount
    writeIORef windowCount (i + 1)

makeWindowCountRef ::
       forall actions. WindowButtons actions
    => IORef Int
    -> UIWindow actions
    -> IO ()
makeWindowCountRef windowCount MkUIWindow {..} =
    makeViewWindowCountRef (getTheView uiwSpec) windowCount uiwTitle uiwSubscriber

truthMain :: ([String] -> IO [SomeUIWindow]) -> IO ()
truthMain getWindows = do
    args <- initGUI
    wms <- getWindows args
    _ <- timeoutAddFull (yield >> return True) priorityDefaultIdle 50
    windowCount <- newIORef 0
    for_ wms $ \(MkSomeUIWindow uiw) -> makeWindowCountRef windowCount uiw
    c <- readIORef windowCount
    if c == 0
        then return ()
        else mainGUI