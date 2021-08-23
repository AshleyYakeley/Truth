module Changes.UI.GTK.Main
    ( changesMainGTK
    ) where

import Changes.Core
import Changes.Core.UI.Toolkit.Run
import GI.GLib as GI hiding (String)
import GI.Gdk as GI (threadsAddIdle)
import GI.Gtk as GI
import Shapes
import Changes.Debug.Reference

data RunState
    = RSRun
    | RSStop

changesMainGTK :: ChangesMain
changesMainGTK appMain = traceBracketIO "THREAD: main" $
    runLifeCycle $
    liftIOWithUnlift $ \unlift -> traceBracketIO "truthMainGTK" $ do
        _ <- GI.init Nothing
        uiLockVar <- newMVar ()
        runVar <- newMVar RSRun
        let
            rtWithLock :: forall a. IO a -> IO a
            rtWithLock = traceBarrier "rtWithLock" $ mVarUnitRun uiLockVar
            rtWithoutLock :: forall a. IO a -> IO a
            rtWithoutLock = traceBarrier "rtWithoutLock" $ mVarUnitUnlock uiLockVar
            rtExit :: IO ()
            rtExit = traceBarrier "truthMainGTK: rtExit" (mVarRun runVar) $ put RSStop
            rtUnliftLifeCycle :: forall a. LifeCycle a -> IO a
            rtUnliftLifeCycle = unlift
            rt = MkRunToolkit {..}
        a <- unlift $ rtRunView rt emptyResourceContext $ quitOnAllClosed rt appMain
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
                traceBarrier "truthMainGTK: mainLoop" (mVarUnitRun uiLockVar) $ #run mloop
        return a
