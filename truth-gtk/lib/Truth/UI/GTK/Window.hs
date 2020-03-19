module Truth.UI.GTK.Window
    ( truthMainGTK
    , getMaybeView
    ) where

import GI.GLib as GI hiding (String)
import GI.Gdk as GI (threadsAddIdle)
import GI.Gtk as GI
import Shapes
import Truth.Core
import Truth.UI.GTK.Button
import Truth.UI.GTK.CSS
import Truth.UI.GTK.Calendar
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
        , calendarGetView
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
        cvBindUpdateFunction Nothing wsTitle $ \title -> set window [#title := title]
        content <- getTheView wsContent
        _ <-
            on window #deleteEvent $ \_ -> traceBracket "GTK.Window:close" $ do
                liftIO wsCloseBoxAction
                return True -- don't run existing handler that closes the window
        ui <-
            case wsMenuBar of
                Nothing -> return content
                Just efmbar -> do
                    ag <- new AccelGroup []
                    #addAccelGroup window ag
                    mb <- switchView $ funcUpdateFunction (\mbar -> createMenuBar ag mbar >>= toWidget) . efmbar aspect
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

data RunState
    = RSRun
    | RSStop

truthMainGTK :: TruthMain
truthMainGTK appMain =
    runLifeCycle $
    liftIOWithUnlift $ \unlift -> traceBracket "truthMainGTK" $ do
        _ <- GI.init Nothing
        uiLockVar <- newMVar ()
        runVar <- newMVar RSRun
        let
            uitWithLock :: forall a. IO a -> IO a
            uitWithLock action = traceBarrier "uitWithLock" (mVarRun uiLockVar) $ liftIO action
            uitCreateWindow :: forall edit. Subscriber edit -> WindowSpec edit -> LifeCycleIO UIWindow
            uitCreateWindow sub wspec = subscribeView uitWithLock (createWindowAndChild wspec) sub getRequest
            uitExit :: IO ()
            uitExit = traceBarrier "truthMainGTK: uitQuit" (mVarRun runVar) $ put RSStop
            uitUnliftLifeCycle :: forall a. LifeCycleIO a -> IO a
            uitUnliftLifeCycle = unlift
            tcUIToolkit = MkUIToolkit {..}
        a <- unlift $ appMain MkTruthContext {..}
        shouldRun <- liftIO $ mVarRun runVar Shapes.get
        case shouldRun of
            RSStop -> return ()
            RSRun -> do
                mloop <- mainLoopNew Nothing False
                _ <-
                    threadsAddIdle PRIORITY_DEFAULT_IDLE $ do
                        putMVar uiLockVar ()
                        threadDelay 5000 -- 5ms delay
                        takeMVar uiLockVar
                        sr <- mVarRun runVar Shapes.get
                        case sr of
                            RSRun -> return SOURCE_CONTINUE
                            RSStop -> do
                                #quit mloop
                                return SOURCE_REMOVE
                traceBarrier "truthMainGTK: pcMainLoop" (mVarRun uiLockVar) $ liftIO $ #run mloop
        return a
