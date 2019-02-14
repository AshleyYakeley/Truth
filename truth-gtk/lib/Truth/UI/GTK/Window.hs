module Truth.UI.GTK.Window
    ( WindowButtons(..)
    , UserInterface(..)
    , truthMain
    , getMaybeView
    ) where

import GI.GLib hiding (String)
import GI.Gtk as GI
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

lastResortView :: UISpec sel edit -> GCreateView sel edit
lastResortView spec = do
    w <- liftIO $ labelNew $ Just $ "missing viewer for " <> fromString (show spec)
    toWidget w

nullGetView :: GetGView
nullGetView =
    MkGetView $ \_ uispec -> do
        MkNullUISpec <- isUISpec uispec
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

getRequest :: forall t. IOWitness t -> Maybe t
getRequest wit = do
    Refl <- testEquality wit witChooseFile
    return $ do
        dialog <- new FileChooserDialog [#action := FileChooserActionOpen]
        _ <- #addButton dialog "Cancel" $ fromIntegral $ fromEnum ResponseTypeCancel
        _ <- #addButton dialog "Copy" $ fromIntegral $ fromEnum ResponseTypeOk
        res <- #run dialog
        mpath <-
            case toEnum $ fromIntegral res of
                ResponseTypeOk -> fileChooserGetFilename dialog
                _ -> return Nothing
        #destroy dialog
        return mpath

getMaybeView :: UISpec sel edit -> Maybe (GCreateView sel edit)
getMaybeView = getUIView allGetView getTheView

getTheView :: UISpec sel edit -> GCreateView sel edit
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
       WindowButtons actions
    => WindowSpec edit
    -> IO ()
    -> (forall sel. CreateView sel edit (actions -> LifeCycle UIWindow) -> r)
    -> r
createWindowAndChild MkWindowSpec {..} closeWindow cont =
    cont $ do
        window <-
            lcNewDestroy Window [#windowPosition := WindowPositionCenter, #defaultWidth := 300, #defaultHeight := 400]
        cvBindEditFunction wsTitle $ \title -> set window [#title := title]
        content <- getTheView wsContent
        _ <-
            on window #deleteEvent $ \_ -> do
                liftIO closeWindow
                return True -- don't run existing handler that closes the window
        menubar <- menuBarNew
        fileMI <- liftIO $ attachMenuItem menubar "File"
        fileMenu <- liftIO $ attachSubmenu fileMI
        closeMI <- liftIO $ attachMenuItem fileMenu "Close"
        liftIO $ menuItemAction closeMI closeWindow
        return $ \actions -> do
            box <- new Box [#orientation := OrientationVertical]
            #packStart box menubar False False 0
            addButtons box actions
            -- this is only correct if content has native scroll support, such as TextView
            sw <- new ScrolledWindow []
            scrollable <- liftIO $ isScrollable content
            if scrollable
                then #add sw content
                else do
                    viewport <- new Viewport []
                    #add viewport content
                    #add sw viewport
            #packStart box sw True True 0
            #add window box
            #show content
            #showAll window
            let uiWindowClose = closeWindow
            return $ MkUIWindow {..}

data UserInterface specifier = forall edit actions. WindowButtons actions =>
                                                        MkUserInterface
    { userinterfaceSubscriber :: Subscriber edit actions
    , userinterfaceSpecifier :: specifier edit
    }

makeViewWindow :: IO () -> UserInterface WindowSpec -> IO UIWindow
makeViewWindow tellclose (MkUserInterface (sub :: Subscriber edit actions) (window :: WindowSpec edit)) = do
    rec
        (uiwindow, closer) <-
            createWindowAndChild window (closer >> tellclose) $ \cv ->
                runLifeCycle $ do
                    (followUp, action) <- subscribeView' cv sub getRequest
                    followUp action
    return uiwindow

data ProgramContext = MkProgramContext
    { pcMainLoop :: MainLoop
    , pcWindowCount :: MVar Int
    }

makeWindowCountRef :: ProgramContext -> UserInterface WindowSpec -> IO UIWindow
makeWindowCountRef MkProgramContext {..} ui = let
    closer =
        mvarRun pcWindowCount $ do
            i <- Shapes.get
            Shapes.put $ i - 1
            if i == 1
                then #quit pcMainLoop
                else return ()
    in mvarRun pcWindowCount $ do
           twindow <- lift $ makeViewWindow closer ui
           i <- Shapes.get
           Shapes.put $ i + 1
           return twindow

truthMain :: ([String] -> (UserInterface WindowSpec -> IO UIWindow) -> LifeCycle ()) -> IO ()
truthMain appMain = do
    args <- getArgs
    _ <- GI.init Nothing
    pcMainLoop <- mainLoopNew Nothing False
    -- _ <- timeoutAddFull (yield >> return True) priorityDefaultIdle 50
    pcWindowCount <- newMVar 0
    withLifeCycle (appMain args (\uiw -> makeWindowCountRef MkProgramContext {..} uiw)) $ \() -> do
        c <- mvarRun pcWindowCount $ Shapes.get
        if c == 0
            then return ()
            else #run pcMainLoop
