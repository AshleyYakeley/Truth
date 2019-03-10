module Truth.UI.GTK.Window
    ( UserInterface(..)
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
        , menuBarGetView
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
        return $ do
            #add window content
            #show content
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
    -- _ <- timeoutAdd PRIORITY_DEFAULT 50 (yield >> return True)
    pcWindowCount <- newMVar 0
    withLifeCycle (appMain args (\uiw -> makeWindowCountRef MkProgramContext {..} uiw)) $ \() -> do
        c <- mvarRun pcWindowCount $ Shapes.get
        if c == 0
            then return ()
            else #run pcMainLoop
