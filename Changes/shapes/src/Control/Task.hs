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
    , raceTasks
    , parallelFor
    , parallelFor_
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

mvarTask :: forall a. MVar a -> Task IO a
mvarTask var = let
    taskWait :: IO a
    taskWait = readMVar var
    taskIsDone :: IO Bool
    taskIsDone = fmap isJust $ tryReadMVar var
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
    return $ execTask $ fmap (liftTunnelIO . fromResultExc) $ hoistTask liftIO $ mvarTask var

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

raceTasks :: MonadTunnelIO m => [Task m a] -> m (Task m a)
raceTasks tt = do
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
