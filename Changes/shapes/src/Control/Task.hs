module Control.Task
    ( Task(..)
    , hoistTask
    , checkTask
    , execTask
    , ioTask
    , forkTask
    , mvarTask
    , timeTask
    , durationTask
    , firstTask
    , parallelFor
    , parallelFor_
    , Cancelled(..)
    , forkEndlessInLifecycle
    , StoppableTask(..)
    , forkStoppableTask
    , foreverStoppableTask
    , followStoppableTask
    , firstStoppableTask
    , raceStoppableTasks
    ) where

import Shapes.Import

type Task :: (Type -> Type) -> Type -> Type
data Task m a = MkTask
    { taskWait :: m a
    , taskIsDone :: m Bool
    }

instance Functor m => Functor (Task m) where
    fmap ab (MkTask w d) = MkTask (fmap ab w) d

instance Applicative m => Applicative (Task m) where
    pure a = MkTask (pure a) (pure True)
    (MkTask wab dab) <*> (MkTask wa da) = MkTask (wab <*> wa) ((&&) <$> dab <*> da)

instance (Applicative m, Semigroup a) => Semigroup (Task m a) where
    (<>) = liftA2 (<>)

instance (Applicative m, Monoid a) => Monoid (Task m a) where
    mempty = pure mempty

hoistTask :: (m1 --> m2) -> Task m1 --> Task m2
hoistTask mm (MkTask w d) = MkTask (mm w) (mm d)

{-
instance RepresentationalRole m => RepresentationalRole (Task m) where
    representationalCoercion cab = case representationalCoercion @_ @_ @m cab of
        MkCoercion -> MkCoercion
-}
ioTask ::
       forall m a. Monad m
    => m (Task m a)
    -> Task m a
ioTask mt =
    MkTask
        { taskWait =
              do
                  t <- mt
                  taskWait t
        , taskIsDone =
              do
                  t <- mt
                  taskIsDone t
        }

mvarTask ::
       forall m a. MonadIO m
    => MVar a
    -> Task m a
mvarTask var = let
    taskWait :: m a
    taskWait = liftIO $ readMVar var
    taskIsDone :: m Bool
    taskIsDone = liftIO $ fmap isJust $ tryReadMVar var
    in MkTask {..}

execTask :: Monad m => Task m (m a) -> Task m a
execTask (MkTask w isd) = MkTask (w >>= id) isd

tunnelForkIO :: MonadTunnelIO m => ((forall a. m a -> IO (TunnelIO m a)) -> IO ()) -> m ThreadId
tunnelForkIO iou =
    tunnelIO $ \unliftIO -> do
        tid <- forkIO $ iou unliftIO
        return $ pure tid

liftTunnelIO :: MonadTunnelIO m => IO (TunnelIO m r) -> m r
liftTunnelIO iomr = tunnelIO $ \_ -> iomr

forkTask :: MonadTunnelIO m => m a -> m (Task m a)
forkTask ma = do
    var <- liftIO newEmptyMVar
    _ <-
        tunnelForkIO $ \unliftIO -> do
            ra <- tryExc $ unliftIO ma
            putMVar var ra
    return $ execTask $ fmap (liftTunnelIO . fromResultExc) $ mvarTask var

checkTask :: Monad m => Task m a -> m (Maybe a)
checkTask task = do
    done <- taskIsDone task
    if done
        then fmap Just $ taskWait task
        else return Nothing

timeTask :: UTCTime -> Task IO ()
timeTask t = let
    remaining :: IO (Maybe NominalDiffTime)
    remaining = do
        c <- getCurrentTime
        let d = diffUTCTime c t
        return $
            if d > 0
                then Just d
                else Nothing
    taskWait :: IO ()
    taskWait = do
        r <- remaining
        case r of
            Just d -> threadSleep d
            Nothing -> return ()
    taskIsDone :: IO Bool
    taskIsDone = do
        d <- remaining
        return $ not $ isJust d
    in MkTask {..}

durationTask :: NominalDiffTime -> IO (Task IO ())
durationTask d = do
    c <- getCurrentTime
    return $ timeTask $ addUTCTime d c

-- | Return a task for the first task to finish. Does not stop the other tasks.
firstTask :: MonadTunnelIO m => [Task m a] -> m (Task m a)
firstTask tt = do
    var <- liftIO newEmptyMVar
    for_ tt $ \task -> do
        _ <-
            tunnelForkIO $ \unliftIO -> do
                a <- unliftIO $ taskWait task
                _ <- tryPutMVar var a
                return ()
        return ()
    return MkTask {taskIsDone = shortOr $ fmap taskIsDone tt, taskWait = liftTunnelIO $ takeMVar var}

parallelFor :: (Traversable t, MonadTunnelIO m) => t a -> (a -> m b) -> m (t b)
parallelFor ta amb = do
    tasks <- for ta $ \a -> forkTask $ amb a
    for tasks taskWait

parallelFor_ :: (Traversable t, MonadTunnelIO m) => t a -> (a -> m ()) -> m ()
parallelFor_ ta amb = do
    tasks <- for ta $ \a -> forkTask $ amb a
    for_ tasks taskWait

data Cancelled =
    MkCancelled
    deriving (Show)

instance Exception Cancelled

-- | Run in another thread, cancelling at the end of the lifecycle
forkEndlessInLifecycle :: IO () -> Lifecycle ()
forkEndlessInLifecycle mm = do
    thread <- liftIO $ forkIO $ handle (\MkCancelled -> return ()) mm
    lifecycleOnClose $ throwTo thread MkCancelled

type StoppableTask :: (Type -> Type) -> Type -> Type
data StoppableTask m a = MkStoppableTask
    { stoppableTaskTask :: Task m (Maybe a)
    , stoppableTaskStop :: m ()
    }

instance Functor m => Functor (StoppableTask m) where
    fmap ab (MkStoppableTask t s) = MkStoppableTask (fmap (fmap ab) t) s

instance Applicative m => Applicative (StoppableTask m) where
    pure a = MkStoppableTask (pure $ pure a) (pure ())
    (MkStoppableTask tab sab) <*> (MkStoppableTask ta sa) = MkStoppableTask (liftA2 (<*>) tab ta) (sab *> sa)

instance (Applicative m, Semigroup a) => Semigroup (StoppableTask m a) where
    (<>) = liftA2 (<>)

instance (Applicative m, Monoid a) => Monoid (StoppableTask m a) where
    mempty = pure mempty

forkStoppableTask ::
       forall m a. MonadTunnelIO m
    => ((forall r. m r) -> m a)
    -> m (StoppableTask m a)
forkStoppableTask ma = do
    var <- liftIO newEmptyMVar
    tid <-
        tunnelForkIO $ \unliftIO -> do
            ra <- tryExc $ unliftIO $ ma $ liftIO $ throw MkCancelled
            putMVar var $
                case ra of
                    SuccessResult ta -> SuccessResult $ fmap Just ta
                    FailureResult exc
                        | Just MkCancelled <- fromException exc -> SuccessResult $ pure Nothing
                    FailureResult exc -> FailureResult exc
    let
        stoppableTaskTask = execTask $ fmap (liftTunnelIO . fromResultExc) $ mvarTask var
        stoppableTaskStop = liftIO $ throwTo tid MkCancelled
    return MkStoppableTask {..}

-- | Create a stoppable task that will run forever (until stopped).
foreverStoppableTask ::
       forall m a. MonadIO m
    => IO (StoppableTask m a)
foreverStoppableTask = do
    var <- newEmptyMVar
    let
        stop :: m ()
        stop = do
            _ <- liftIO $ tryPutMVar var Nothing
            return ()
    return $ MkStoppableTask (mvarTask var) stop

-- | Create a StoppableTask that follows a Task. Stopping it will not stop the original Task.
followStoppableTask ::
       forall m a. MonadTunnelIO m
    => Task m a
    -> m (StoppableTask m a)
followStoppableTask task = do
    MkStoppableTask ftask stop <- liftIO foreverStoppableTask
    rtask <- firstTask [fmap Just task, ftask]
    return $ MkStoppableTask rtask stop

-- | Create a StoppableTask for the first StoppableTask to finish. Does not stop the other tasks when done.
firstStoppableTask ::
       forall m a. MonadTunnelIO m
    => [StoppableTask m a]
    -> m (StoppableTask m a)
firstStoppableTask stasks = do
    task <- firstTask $ fmap stoppableTaskTask stasks
    let
        stop :: m ()
        stop = for_ stasks stoppableTaskStop
    return $ MkStoppableTask task stop

-- | Create a StoppableTask for the first StoppableTask to finish. Also forks a task to stop them all when that happens.
raceStoppableTasks ::
       forall m a. MonadTunnelIO m
    => [StoppableTask m a]
    -> m (StoppableTask m a)
raceStoppableTasks stasks = do
    MkStoppableTask ftask stop <- firstStoppableTask stasks
    rtask <-
        forkTask $ do
            ma <- taskWait ftask
            for_ stasks stoppableTaskStop
            return ma
    return $ MkStoppableTask rtask stop
