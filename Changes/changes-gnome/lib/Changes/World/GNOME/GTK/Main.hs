module Changes.World.GNOME.GTK.Main
    ( runGTK
    , waitUntilDoneGTK
    , startGTK
    )
where

import Data.IORef

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

runInIdle :: Maybe GI.MainContext -> IO () -> IO ()
runInIdle mmc work = do
    source <- GI.idleSourceNew
    GI.sourceSetPriority source GI.PRIORITY_DEFAULT_IDLE
    GI.sourceSetCallback source $ do
        work
        return GI.SOURCE_REMOVE
    _ <- GI.sourceAttach source $ mmc
    return ()

mainLoop :: Maybe GI.MainContext -> TPieceVar (IO ()) -> TPieceVar () -> IO ()
mainLoop mmc workVar doneVar = do
    exitRef <- newIORef False
    let
        checkExit :: IO () -> IO ()
        checkExit call = do
            mExit <- readIORef exitRef
            case mExit of
                True -> return ()
                False -> call

    _ <- forkIO $ do
        pipe (atomically $ ppPull $ workVar <+++> doneVar) (runInIdle mmc)
        runInIdle mmc $ writeIORef exitRef True
    checkExit $ do
        let
            go :: IO ()
            go = checkExit $ do
                _ <- GI.mainContextIteration mmc True
                go
        go

startGTK :: MonadIO m => LifecycleT m m GTKContext
startGTK = do
    threadDoneVar <- liftIO newEmptyMVar
    doneVar <- liftIO $ atomically mkWholeTPieceVar
    context <- liftIO $ do
        exitVar <- newEmptyMVar
        workVar <- atomically mkWholeTPieceVar
        mc <- GI.mainContextDefault
        _ <- forkOS $ do
            ra <- tryExc $ do
                GI.init
                let
                    mmc = Just mc
                mainLoop mmc workVar doneVar
            putMVar threadDoneVar ra
        let
            gtkcExit :: Result (Exc IO) () -> IO ()
            gtkcExit ra = void $ tryPutMVar exitVar ra

        (ondone, checkdone) <- lifecycleOnAllDone $ gtkcExit $ SuccessResult ()
        let
            gtkcWaitForExit :: IO (Result (Exc IO) ())
            gtkcWaitForExit = do
                checkdone
                readMVar exitVar

            gtkcExitOnClosed :: View ()
            gtkcExitOnClosed = viewLiftLifecycle ondone

            gtkcLock :: SingleThreadLock
            gtkcLock = mkSingleThreadLock workVar $ do
                isBound <- isCurrentThreadBound
                if isBound
                    then GI.mainContextIsOwner mc
                    else return False

            context :: GTKContext
            context = MkGTKContext{..}

        return context
    lifecycleOnClose $ liftIO $ do
        atomically $ ppMaybePush doneVar ()
        ra <- takeMVar threadDoneVar
        fromResultExc ra
    return context

waitUntilDoneGTK :: GTKContext -> IO ()
waitUntilDoneGTK context = do
    ra <- gtkcWaitForExit context
    fromResultExc ra

runGTK :: MonadIO m => (GTKContext -> LifecycleT m m a) -> LifecycleT m m a
runGTK call = do
    context <- startGTK
    r <- call context
    liftIO $ waitUntilDoneGTK context
    return r
