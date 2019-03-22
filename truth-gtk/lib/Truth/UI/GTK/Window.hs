module Truth.UI.GTK.Window
    ( UserInterface(..)
    , TruthContext(..)
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
import Truth.UI.GTK.MenuBar
import Truth.UI.GTK.Option
import Truth.UI.GTK.Pages
import Truth.UI.GTK.Scrolled
import Truth.UI.GTK.Switch
import Truth.UI.GTK.Table
import Truth.UI.GTK.Text
import Truth.UI.GTK.Useful
import Truth.Debug.Object

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
        , scrolledGetView
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

createWindowAndChild :: WindowSpec edit -> IO () -> (forall sel. CreateView sel edit (LifeCycle UIWindow) -> r) -> r
createWindowAndChild MkWindowSpec {..} closeWindow cont =
    cont $ do
        window <-
            lcNewDestroy Window [#windowPosition := WindowPositionCenter, #defaultWidth := 300, #defaultHeight := 400]
        cvBindEditFunction wsTitle $ \title -> set window [#title := title]
        content <- getTheView wsContent
        _ <-
            on window #deleteEvent $ \_ -> traceBracket "GTK.Window:close" $ do
                liftIO closeWindow
                return True -- don't run existing handler that closes the window
        ui <-
            case wsMenuBar of
                Nothing -> return content
                Just efmbar -> do
                    ag <- new AccelGroup []
                    #addAccelGroup window ag
                    mb <- switchView $ funcEditFunction (\mbar -> createMenuBar ag mbar >>= toWidget) . efmbar
                    vbox <- new Box [#orientation := OrientationVertical]
                    #packStart vbox mb False False 0
                    #packStart vbox content True True 0
                    toWidget vbox
        return $ do
            #add window ui
            #show ui
            #showAll window
            let uiWindowClose = closeWindow
            return $ MkUIWindow {..}

data UserInterface specifier = forall edit. MkUserInterface
    { userinterfaceSubscriber :: Subscriber edit
    , userinterfaceSpecifier :: specifier edit
    }

makeViewWindow :: IO () -> UserInterface WindowSpec -> IO UIWindow
makeViewWindow tellclose (MkUserInterface (sub :: Subscriber edit) (window :: WindowSpec edit)) = do
    rec
        (r, closer) <-
            createWindowAndChild window (closer >> tellclose) $ \cv ->
                runLifeCycle $ do
                    followUp <- subscribeView' cv sub getRequest
                    followUp
    return r

data ProgramContext = MkProgramContext
    { pcMainLoop :: MainLoop
    , pcWindowClosers :: MVar (Store (IO ()))
    }

makeWindowCountRef :: ProgramContext -> UserInterface WindowSpec -> IO UIWindow
makeWindowCountRef MkProgramContext {..} ui = let
    closer key =
        mvarRun pcWindowClosers $ do
            oldstore <- Shapes.get
            let newstore = deleteStore key oldstore
            Shapes.put newstore
            if isEmptyStore newstore
                then #quit pcMainLoop
                else return ()
    in mvarRun pcWindowClosers $ do
           oldstore <- Shapes.get
           rec
               twindow <- lift $ makeViewWindow (closer key) ui
               let (key, newstore) = addStore (uiWindowClose twindow) oldstore
           Shapes.put newstore
           return twindow

data TruthContext = MkTruthContext
    { tcArguments :: [String]
    , tcCreateWindow :: UserInterface WindowSpec -> IO UIWindow
    , tcCloseAllWindows :: IO ()
    }

truthMain :: (TruthContext -> LifeCycle ()) -> IO ()
truthMain appMain = do
    tcArguments <- getArgs
    _ <- GI.init Nothing
    pcMainLoop <- mainLoopNew Nothing False
    -- _ <- timeoutAddFull (yield >> return True) priorityDefaultIdle 50
    pcWindowClosers <- newMVar emptyStore
    let
        tcCreateWindow uiw = makeWindowCountRef MkProgramContext {..} uiw
        tcCloseAllWindows = do
            store <- mvarRun pcWindowClosers $ Shapes.get
            for_ store $ \cw -> cw
    withLifeCycle (appMain MkTruthContext {..}) $ \() -> do
        store <- mvarRun pcWindowClosers $ Shapes.get
        if isEmptyStore store
            then return ()
            else #run pcMainLoop
