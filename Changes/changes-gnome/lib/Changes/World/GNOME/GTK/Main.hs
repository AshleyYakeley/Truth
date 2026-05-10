module Changes.World.GNOME.GTK.Main
    ( runGTK
    -- , runGTKAsync
    -- , runGTKAsyncView
    )
where

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

data RunState
    = RSRun
    | RSExit
    | RSException SomeException

runInIdle :: Maybe GI.MainContext -> IO () -> IO ()
runInIdle mmc call = do
    source <- GI.idleSourceNew
    GI.sourceSetPriority source GI.PRIORITY_DEFAULT_IDLE
    GI.sourceSetCallback source $ do
        call
        return GI.SOURCE_REMOVE
    _ <- GI.sourceAttach source $ mmc
    return ()

runVal :: (IO () -> IO ()) -> (IO --> IO)
runVal run ioa = do
    var <- newEmptyMVar
    run $ do
        a <- ioa
        putMVar var a
    takeMVar var

runSafe :: (IO --> IO) -> (IO --> IO)
runSafe run ioa = do
    ra <- run $ tryExc ioa
    fromResultExc ra

mainLoop :: Maybe GI.MainContext -> MVar RunState -> IO ()
mainLoop mmc runVar = do
    shouldRun <- mVarRunStateT runVar get
    case shouldRun of
        RSExit -> return ()
        RSException ex -> throwExc ex
        RSRun -> do
            let
                go :: IO ()
                go = do
                    sr <- mVarRunStateT runVar get
                    case sr of
                        RSRun -> do
                            _ <- GI.mainContextIteration mmc True
                            go
                        RSExit -> return ()
                        RSException ex -> throwExc ex
            go

runGTK :: MonadIO m => (GTKContext 'Unlocked -> m a) -> m a
runGTK call = do
    (gtkContext, after) <- liftIO $ do
        runVar <- newMVar RSRun
        GI.init
        mc <- GI.mainContextDefault
        let
            mmc = Just mc

            runInThread :: IO --> IO
            runInThread = runSafe $ runVal $ runInIdle mmc

            gtkcExit :: IO ()
            gtkcExit = mVarRunStateT runVar $ put RSExit

            gtkcThrow :: SomeException -> IO ()
            gtkcThrow ex = mVarRunStateT runVar $ put $ RSException ex
        gtkcLock :: SingleThreadLock 'Unlocked <- liftIO $ newSingleThreadLock runInThread
        (ondone, checkDone) <- lifecycleOnAllDone gtkcExit
        let
            gtkcExitOnClosed :: View ()
            gtkcExitOnClosed = viewLiftLifecycle ondone

            gtkContext :: GTKContext 'Unlocked
            gtkContext = MkGTKContext{..}

            after :: IO ()
            after = do
                checkDone
                mainLoop mmc runVar
        return (gtkContext, after)
    a <- call gtkContext
    liftIO after
    return a

{-
runGTKAsync :: Lifecycle (GTKContext 'Unlocked)
runGTKAsync = do
    gtkcLock <- liftIO $ newSingleThreadLock runInThread
    runVar <- liftIO $ newMVar RSRun
    let
        gtkcExit :: IO ()
        gtkcExit = mVarRunStateT runVar $ put RSExit
        gtkcThrow :: SomeException -> IO ()
        gtkcThrow ex = mVarRunStateT runVar $ put $ RSException ex
    (ondone, _checkdone) <- liftIO $ lifecycleOnAllDone gtkcExit
    let
        gtkcExitOnClosed :: View ()
        gtkcExitOnClosed = viewLiftLifecycle ondone
    initDoneVar <- liftIO newEmptyMVar
    (task, _threadId) <- forkOSTask $ runLocked gtkcLock $ \lock' -> do
        GI.init
        liftIO $ putMVar initDoneVar ()
        mainLoop lock' runVar
    liftIO $ takeMVar initDoneVar
    lifecycleOnClose $ runLifecycle $ taskWait task
    return MkGTKContext{..}

runGTKAsyncView :: View (GTKContext 'Unlocked)
runGTKAsyncView = viewLiftLifecycle runGTKAsync
-}
