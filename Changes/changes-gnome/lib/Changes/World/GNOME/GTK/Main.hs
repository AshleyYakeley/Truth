module Changes.World.GNOME.GTK.Main
    ( runGTK
    , runGTKView
    ) where

import Changes.Core
import Changes.World.GNOME.GI
import GI.GLib as GI hiding (String)
import GI.Gtk as GI
import Shapes

data RunState
    = RSRun
    | RSStop

attachedIdleSource :: Maybe MainContext -> Lifecycle GI.Source
attachedIdleSource mmc = do
    source <- idleSourceNew
    _ <- sourceAttach source $ mmc
    lifecycleOnClose $ sourceDestroy source
    return source

mainLoop :: CallbackLock -> MVar RunState -> Lifecycle ()
mainLoop uiLock runVar = do
    shouldRun <- mVarRunStateT runVar Shapes.get
    case shouldRun of
        RSStop -> return ()
        RSRun -> do
            mc <- mainContextDefault
            source <- attachedIdleSource $ Just mc
            sourceSetCallback source $ do
                cbRunUnlocked uiLock $ threadDelay 5000 -- 5ms delay
                return SOURCE_CONTINUE
            let
                mainloop :: IO ()
                mainloop = do
                    sr <- mVarRunStateT runVar Shapes.get
                    case sr of
                        RSRun -> do
                            _ <- mainContextIteration mc True
                            mainloop
                        RSStop -> return ()
            liftIO $ cbRunLocked uiLock mainloop

runGTK :: forall a. (GTKContext -> Lifecycle a) -> Lifecycle a
runGTK call = do
    _ <- GI.init Nothing
    gtkcLock <- liftIO newCallbackLock
    runVar <- liftIO $ newMVar RSRun
    let
        gtkcExit :: IO ()
        gtkcExit = mVarRunStateT runVar $ put RSStop
    (ondone, checkdone) <- liftIO $ lifecycleOnAllDone gtkcExit
    let
        gtkcExitOnClosed :: View --> View
        gtkcExitOnClosed ma = do
            viewLiftLifecycle ondone
            ma
    a <- call MkGTKContext {..}
    liftIO checkdone
    mainLoop gtkcLock runVar
    return a

runGTKView :: forall a. (GTKContext -> View a) -> View a
runGTKView call = viewLiftLifecycleWithUnlift $ \unlift -> runGTK $ \gtkc -> unlift $ call gtkc
