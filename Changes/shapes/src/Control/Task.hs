module Control.Task where

import Shapes.Import

data Task a = MkTask
    { taskWait :: IO a
    , taskIsDone :: IO Bool
    }

instance Functor Task where
    fmap ab (MkTask w d) = MkTask (fmap ab w) d

instance Applicative Task where
    pure a = MkTask (return a) (return True)
    (MkTask wab dab) <*> (MkTask wa da) = MkTask (wab <*> wa) ((&&) <$> dab <*> da)

instance Semigroup a => Semigroup (Task a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (Task a) where
    mempty = pure mempty

ioTask :: IO (Task a) -> Task a
ioTask iot =
    MkTask
        { taskWait =
              do
                  t <- iot
                  taskWait t
        , taskIsDone =
              do
                  t <- iot
                  taskIsDone t
        }

singleTask :: forall a. IO a -> IO (IO (), Task a)
singleTask ioa = do
    var <- newEmptyMVar
    let
        action :: IO ()
        action = do
            rea <- tryExc ioa
            putMVar var rea
        taskWait :: IO a
        taskWait = do
            rea <- readMVar var
            throwResult rea
        taskIsDone :: IO Bool
        taskIsDone = fmap isJust $ tryReadMVar var
        task = MkTask {..}
    return (action, task)

forkSingleTask :: IO a -> IO (Task a)
forkSingleTask ioa = do
    (action, task) <- singleTask ioa
    _ <- forkIO action
    return task

taskNotify :: Task a -> (a -> IO ()) -> IO ()
taskNotify task call = do
    _ <-
        forkIO $ do
            a <- taskWait task
            call a
    return ()

quickTask :: IO a -> Task a
quickTask ioa = let
    taskWait = ioa
    taskIsDone = return True
    in MkTask {..}
