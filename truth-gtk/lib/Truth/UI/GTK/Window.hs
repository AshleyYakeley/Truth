module Truth.UI.GTK.Window
    ( truthMainGTK
    , getMaybeView
    ) where

import GI.GLib as GI hiding (String)
import GI.Gdk as GI (threadsAddIdle)
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

createWindowAndChild :: WindowSpec edit -> AnyCreateView edit UIWindow
createWindowAndChild MkWindowSpec {..} =
    MkAnyCreateView $
    cvWithAspect $ \aspect -> do
        window <-
            lcNewDestroy Window [#windowPosition := WindowPositionCenter, #defaultWidth := 300, #defaultHeight := 400]
        cvBindEditFunction Nothing wsTitle $ \title -> set window [#title := title]
        content <- getTheView wsContent
        _ <-
            on window #deleteEvent $ \_ -> do
                liftIO wsCloseBoxAction
                return True -- don't run existing handler that closes the window
        ui <-
            case wsMenuBar of
                Nothing -> return content
                Just efmbar -> do
                    ag <- new AccelGroup []
                    #addAccelGroup window ag
                    mb <- switchView $ funcEditFunction (\mbar -> createMenuBar ag mbar >>= toWidget) . efmbar aspect
                    vbox <- new Box [#orientation := OrientationVertical]
                    #packStart vbox mb False False 0
                    #packStart vbox content True True 0
                    toWidget vbox
        #add window ui
        #show ui
        #showAll window
        let
            uiWindowHide = #hide window
            uiWindowShow = #show window
        return $ MkUIWindow {..}

{-
forkTask :: IO a -> IO (IO a)
forkTask action = do
    var <- newEmptyMVar
    _ <- forkIO $ do
        a <- action
        putMVar var a
    return $ takeMVar var
-}
data RunState
    = Running
    | Stopped

truthMainGTK :: TruthMain
truthMainGTK appMain =
    runLifeCycle $
    liftIOWithUnlift $ \(MkTransform unlift) -> do
        tcArguments <- getArgs
        _ <- GI.init Nothing
        gtkLockVar <- newMVar ()
        runVar <- newMVar Running
        let
            {-
            uitForkTask :: IO () -> IO ()
            uitForkTask action = do
                waitFinish <- forkTask action
                mvarRun tasksVar $ do
                    finishers <- Shapes.get
                    put $ finishers >> waitFinish
            uitFinishTasks :: IO ()
            uitFinishTasks = do
                finishers <- mvarRun tasksVar $ do
                    f <- Shapes.get
                    put $ return ()
                    return f
                finishers
            -}
            uitWithLock :: forall a. IO a -> IO a
            uitWithLock action = mvarRun gtkLockVar $ liftIO action
            threadBarrier :: Bool -> IO a -> IO a
            threadBarrier True = uitWithLock
            threadBarrier False = id
            uitCreateWindow :: forall edit. Subscriber edit -> WindowSpec edit -> LifeCycleIO UIWindow
            uitCreateWindow sub wspec = subscribeView threadBarrier (createWindowAndChild wspec) sub getRequest
            uitQuit :: IO ()
            uitQuit = mvarRun runVar $ put Stopped
            uitUnliftLifeCycle :: forall a. LifeCycleIO a -> IO a
            uitUnliftLifeCycle = unlift
            tcUIToolkit = MkUIToolkit {..}
        a <- unlift $ appMain MkTruthContext {..}
        shouldRun <- liftIO $ mvarRun runVar Shapes.get
        case shouldRun of
            Stopped -> return ()
            Running -> do
                mloop <- mainLoopNew Nothing False
                _ <-
                    threadsAddIdle PRIORITY_DEFAULT_IDLE $ do
                        putMVar gtkLockVar ()
                        yield
                        takeMVar gtkLockVar
                        sr <- mvarRun runVar Shapes.get
                        case sr of
                            Running -> return SOURCE_CONTINUE
                            Stopped -> do
                                #quit mloop
                                return SOURCE_REMOVE
                mvarRun gtkLockVar $ liftIO $ #run mloop
        return a
