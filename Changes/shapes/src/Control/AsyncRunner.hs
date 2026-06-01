module Control.AsyncRunner
    ( Pusher
    , forkIOPusher
    , forkOSPusher
    , pusherWait
    , asyncWaitRunner
    , asyncRunner
    , asyncIORunner
    , pusherTask
    )
where

import Control.Task
import Shapes.Import

type Pusher = IO () -> IO ()

forkIOPusher :: Pusher
forkIOPusher iou = void $ forkIO iou

forkOSPusher :: Pusher
forkOSPusher iou = void $ forkOS iou

pusherWaitRaw :: Pusher -> (IO --> IO)
pusherWaitRaw run ioa = do
    var <- newEmptyMVar
    run $ do
        a <- ioa
        putMVar var a
    takeMVar var

runSafe :: (IO --> IO) -> (IO --> IO)
runSafe run ioa = do
    ra <- run $ tryExc ioa
    fromResultExc ra

pusherWait :: Pusher -> (IO --> IO)
pusherWait ioa = runSafe $ pusherWaitRaw ioa

pusherTask :: forall a. Pusher -> (IO a -> IO (Task IO a))
pusherTask pushVal ma = do
    (report, task) <- mkTask
    pushVal $ do
        a <- ma
        report a
    pure task

asyncIORunner :: Text -> Lifecycle (Pusher, Task IO ())
asyncIORunner _ = do
    var <- liftIO $ newMVar mempty
    let
        pushVal :: Pusher
        pushVal job =
            mVarRunStateT var $ do
                oldTask <- get
                (newTask, _) <-
                    lift
                        $ forkTask
                        $ do
                            taskWait oldTask
                            job
                put newTask
        utask :: Task IO ()
        utask = ioTask $ mVarRunStateT var get
    lifecycleOnClose $ taskWait utask
    return (pushVal, utask)

newtype SemigroupQueue t
    = MkSemigroupQueue (MVar (Maybe t))

newSemigroupQueue :: IO (SemigroupQueue t)
newSemigroupQueue = do
    var <- newMVar Nothing
    return $ MkSemigroupQueue var

takeSemigroupQueue :: SemigroupQueue t -> IO (Maybe t)
takeSemigroupQueue (MkSemigroupQueue var) =
    mVarRunStateT var $ do
        mt <- get
        put Nothing
        return mt

putSemigroupQueue :: Semigroup t => SemigroupQueue t -> t -> IO Bool
putSemigroupQueue (MkSemigroupQueue var) t =
    mVarRunStateT var $ do
        get >>= \case
            Just oldt -> do
                put $ Just $ oldt <> t
                return False
            Nothing -> do
                put $ Just t
                return True

semigroupPusher :: Semigroup t => Pusher -> (t -> IO ()) -> IO (t -> IO ())
semigroupPusher pushVal doit = do
    sq :: SemigroupQueue t <- newSemigroupQueue
    let
        action :: IO ()
        action = do
            mt <- takeSemigroupQueue sq
            case mt of
                Just t -> doit t
                Nothing -> return ()
        tio :: t -> IO ()
        tio t = do
            b <- putSemigroupQueue sq t
            if b
                then pushVal action
                else return ()
    return tio

semigroupWaitPusher :: Semigroup t => Pusher -> Int -> (t -> IO ()) -> IO (Maybe t -> IO ())
semigroupWaitPusher pushVal mus doit = do
    sq :: SemigroupQueue t <- newSemigroupQueue
    let
        action :: IO ()
        action = do
            mt <- takeSemigroupQueue sq
            case mt of
                Just t -> doit t
                Nothing -> return ()
        mtio :: Maybe t -> IO ()
        mtio (Just t) = do
            b <- putSemigroupQueue sq t
            if b
                then pushVal $ do
                    threadDelay mus
                    action
                else return ()
        mtio Nothing = pushVal action
    return mtio

asyncRunner ::
    forall t.
    Semigroup t =>
    Text ->
    (t -> IO ()) ->
    Lifecycle (t -> IO (), Task IO ())
asyncRunner name doit = do
    (pushVal, utask) <- asyncIORunner name
    tio <- liftIO $ semigroupPusher pushVal doit
    return (tio, utask)

asyncWaitRunner ::
    forall t.
    Semigroup t =>
    Text ->
    Int ->
    (t -> IO ()) ->
    Lifecycle (Maybe t -> IO (), Task IO ())
asyncWaitRunner name mus doit = do
    (pushVal, utask) <- asyncIORunner name
    mtio <- liftIO $ semigroupWaitPusher pushVal mus doit
    return (mtio, utask)
