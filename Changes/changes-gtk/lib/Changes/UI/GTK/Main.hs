module Changes.UI.GTK.Main
    ( runGTK
    , runGTKView
    ) where

import Changes.Core
import Changes.GI
import GI.GLib as GI hiding (String)
import GI.Gdk as GI (threadsAddIdle)
import GI.Gtk as GI
import Shapes

data RunState
    = RSRun
    | RSStop

mainLoop :: CallbackLock -> MVar RunState -> IO ()
mainLoop uiLock runVar = do
    shouldRun <- mVarRun runVar Shapes.get
    case shouldRun of
        RSStop -> return ()
        RSRun -> do
            mc <- mainContextDefault
            -- mloop <- mainLoopNew Nothing False
            _ <-
                threadsAddIdle PRIORITY_DEFAULT_IDLE $ do
                    cbRunUnlocked uiLock $ threadDelay 5000 -- 5ms delay
                    return SOURCE_CONTINUE
            let
                mainloop :: IO ()
                mainloop = do
                    sr <- mVarRun runVar Shapes.get
                    case sr of
                        RSRun -> do
                            _ <- mainContextIteration mc True
                            mainloop
                        RSStop -> return ()
            cbRunLocked uiLock mainloop

runGTK :: forall a. (GTKContext -> LifeCycle a) -> LifeCycle a
runGTK call = do
    _ <- GI.init Nothing
    gtkcLock <- liftIO newCallbackLock
    runVar <- liftIO $ newMVar RSRun
    let
        gtkcExit :: IO ()
        gtkcExit = mVarRun runVar $ put RSStop
    (ondone, checkdone) <- liftIO $ lifeCycleOnAllDone gtkcExit
    let
        gtkcExitOnClosed :: View --> View
        gtkcExitOnClosed ma = do
            viewLiftLifeCycle ondone
            ma
    a <- call MkGTKContext {..}
    liftIO $ do
        checkdone
        mainLoop gtkcLock runVar
    return a

runGTKView :: forall a. (GTKContext -> View a) -> View a
runGTKView call = viewLiftLifeCycleWithUnlift $ \unlift -> runGTK $ \gtkc -> unlift $ call gtkc
