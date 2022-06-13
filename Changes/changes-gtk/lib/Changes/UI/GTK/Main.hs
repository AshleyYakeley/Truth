module Changes.UI.GTK.Main
    ( runGTK
    ) where

import Changes.Core
import Changes.GI.GView
import GI.GLib as GI hiding (String)
import GI.Gdk as GI (threadsAddIdle)
import GI.Gtk as GI
import Shapes

data RunState
    = RSRun
    | RSStop

mainLoop :: MVar () -> MVar RunState -> IO ()
mainLoop uiLockVar runVar = do
    shouldRun <- mVarRun runVar Shapes.get
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
            mVarUnitRun uiLockVar $ #run mloop

runGTK :: LifeCycle GTKContext
runGTK = do
    _ <- GI.init Nothing
    uiLockVar <- liftIO $ newMVar ()
    runVar <- liftIO $ newMVar RSRun
    let
        gtkcExit :: IO ()
        gtkcExit = mVarRun runVar $ put RSStop
        gtkcLockVar = uiLockVar
    (ondone, checkdone) <- liftIO $ lifeCycleOnAllDone gtkcExit
    let
        gtkcExitOnClosed :: View --> View
        gtkcExitOnClosed ma = do
            viewLiftLifeCycle ondone
            ma
    lifeCycleOnCloseIO $ do
        checkdone
        mainLoop uiLockVar runVar
    return MkGTKContext {..}
