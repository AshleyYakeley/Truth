module Changes.World.GNOME.GTK.Main
    ( runGTK
    , runGTKView
    )
where

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

data RunState
    = RSRun
    | RSStop

attachedIdleSource :: Maybe GI.MainContext -> Lifecycle GI.Source
attachedIdleSource mmc = do
    source <- GI.idleSourceNew
    _ <- GI.sourceAttach source $ mmc
    lifecycleOnClose $ GI.sourceDestroy source
    return source

mainLoop :: CallbackLock -> MVar RunState -> Lifecycle ()
mainLoop uiLock runVar = do
    shouldRun <- mVarRunStateT runVar get
    case shouldRun of
        RSStop -> return ()
        RSRun -> do
            mc <- GI.mainContextDefault
            source <- attachedIdleSource $ Just mc
            GI.sourceSetCallback source $ do
                cbRunUnlocked uiLock $ threadDelay 5000 -- 5ms delay
                return GI.SOURCE_CONTINUE
            let
                mainloop :: IO ()
                mainloop = do
                    sr <- mVarRunStateT runVar get
                    case sr of
                        RSRun -> do
                            _ <- GI.mainContextIteration (Just mc) True
                            mainloop
                        RSStop -> return ()
            liftIO mainloop

runGTK :: forall a. (GTKContext -> Lifecycle a) -> Lifecycle a
runGTK call = do
    GI.init
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
    hoistIO (cbRunLocked gtkcLock) $ do
        a <- hoistIO (cbRunUnlocked gtkcLock) $ call MkGTKContext{..}
        liftIO checkdone
        mainLoop gtkcLock runVar
        return a

runGTKView :: forall a. (GTKContext -> View a) -> View a
runGTKView call = viewLiftLifecycleWithUnlift $ \unlift -> runGTK $ \gtkc -> unlift $ call gtkc
