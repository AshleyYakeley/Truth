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

createWindowAndChild :: WindowSpec edit -> IO () -> AnyCreateView edit (LifeCycle UIWindow)
createWindowAndChild MkWindowSpec {..} closeWindow =
    MkAnyCreateView $
    cvWithAspect $ \aspect -> do
        window <-
            lcNewDestroy Window [#windowPosition := WindowPositionCenter, #defaultWidth := 300, #defaultHeight := 400]
        cvBindEditFunction Nothing wsTitle $ \title -> set window [#title := title]
        content <- getTheView wsContent
        _ <-
            on window #deleteEvent $ \_ -> do
                liftIO closeWindow
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
        return $ do
            #add window ui
            #show ui
            #showAll window
            let uiWindowClose = closeWindow
            return $ MkUIWindow {..}

data ProgramContext = MkProgramContext
    { pcMainLoop :: MainLoop
    , pcCallFromOtherThread :: forall a. IO a -> IO a
    , pcWindowClosers :: MVar (Store (IO ()))
    }

threadBarrier :: ProgramContext -> Bool -> IO a -> IO a
threadBarrier MkProgramContext {..} True = pcCallFromOtherThread
threadBarrier MkProgramContext {..} False = id

makeViewWindow :: ProgramContext -> IO () -> Subscriber edit -> WindowSpec edit -> IO UIWindow
makeViewWindow pc tellclose sub window =
    subscribeView (threadBarrier pc) (\closer -> createWindowAndChild window (closer >> tellclose)) sub getRequest

makeWindowCountRef :: ProgramContext -> Subscriber edit -> WindowSpec edit -> IO UIWindow
makeWindowCountRef pc@MkProgramContext {..} sub window = let
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
               twindow <- lift $ makeViewWindow pc (closer key) sub window
               let (key, newstore) = addStore (uiWindowClose twindow) oldstore
           Shapes.put newstore
           return twindow

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

truthMainGTK :: Bool -> TruthMain
truthMainGTK _pcAsync appMain = do
    tcArguments <- getArgs
    _ <- GI.init Nothing
    pcMainLoop <- mainLoopNew Nothing False
    pcWindowClosers <- newMVar emptyStore
    --tasksVar <- newMVar $ return ()
    rsVar <- newMVar Running
    let
        pcCallFromOtherThread :: forall a. IO a -> IO a
        pcCallFromOtherThread action = mvarRun rsVar $ liftIO action
        pc = MkProgramContext {..}
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
        uitWithLock = threadBarrier pc True
        uitCreateWindow :: forall edit. Subscriber edit -> WindowSpec edit -> IO UIWindow
        uitCreateWindow = makeWindowCountRef pc
        uitCloseAllWindows = do
            store <- mvarRun pcWindowClosers $ Shapes.get
            for_ store $ \cw -> cw
        tcUIToolkit = MkUIToolkit {..}
    withLifeCycle (appMain MkTruthContext {..}) $ \a -> do
        store <- mvarRun pcWindowClosers $ Shapes.get
        if isEmptyStore store
            then return ()
            else do
                _ <-
                    threadsAddIdle PRIORITY_DEFAULT_IDLE $ do
                        running <- #isRunning pcMainLoop
                        putMVar rsVar $
                            if running
                                then Running
                                else Stopped
                        yield
                        rs <- takeMVar rsVar
                        return $
                            case rs of
                                Running -> SOURCE_CONTINUE
                                Stopped -> SOURCE_REMOVE
                mvarRun rsVar $ liftIO $ #run pcMainLoop
        --uitFinishTasks
        return a
