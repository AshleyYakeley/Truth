module Changes.UI.GTK.Main
    ( runGTK
    , runGTKView
    ) where

import Changes.Core
import Changes.Debug
import Changes.GI
import GI.GLib as GI hiding (String)
import GI.Gtk as GI
import Shapes

data RunState
    = RSRun
    | RSStop

attachedIdleSource :: Maybe MainContext -> LifeCycle Source
attachedIdleSource mmc = do
    source <- idleSourceNew
    _ <- sourceAttach source $ mmc
    lifeCycleOnClose $ sourceDestroy source
    return source

mainLoop :: CallbackLock -> MVar RunState -> LifeCycle ()
mainLoop uiLock runVar = do
    shouldRun <- mVarRun runVar Shapes.get
    case shouldRun of
        RSStop -> return ()
        RSRun -> do
            mc <- mainContextDefault
            source <- attachedIdleSource $ Just mc
            sourceSetCallback source $ do
                traceBarrier "loop.lock" (cbRunUnlocked uiLock) $ threadDelay 5000 -- 5ms delay
                return SOURCE_CONTINUE
            let
                mainloop :: IO ()
                mainloop = do
                    sr <- mVarRun runVar Shapes.get
                    case sr of
                        RSRun -> do
                            _ <- traceBracket "iterate" $ mainContextIteration mc True
                            mainloop
                        RSStop -> return ()
            liftIO $ cbRunLocked uiLock mainloop

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
    a <- traceBracket "runGTK.call" $ call MkGTKContext {..}
    liftIO checkdone
    traceBracket "mainLoop" $ mainLoop gtkcLock runVar
    return a

runGTKView :: forall a. (GTKContext -> View a) -> View a
runGTKView call = viewLiftLifeCycleWithUnlift $ \unlift -> runGTK $ \gtkc -> unlift $ call gtkc
