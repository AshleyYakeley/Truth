module Control.Task where

import Shapes.Import
import Debug.ThreadTrace

data Task a = MkTask
    { taskWait :: IO a
    , taskIsDone :: IO (Maybe a)
    }

instance Functor Task where
    fmap ab (MkTask w d) = MkTask (fmap ab w) (fmap (fmap ab) d)

instance Applicative Task where
    pure a = MkTask (return a) (return $ Just a)
    (MkTask wab dab) <*> (MkTask wa da) =
        MkTask (wab <*> wa) (getComposeInner $ (MkComposeInner dab) <*> (MkComposeInner da))

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
            a <- ioa
            putMVar var a
        taskWait :: IO a
        taskWait = readMVar var
        taskIsDone :: IO (Maybe a)
        taskIsDone = tryReadMVar var
        task = MkTask {..}
    return (action, task)

forkSingleTask :: IO a -> IO (Task a)
forkSingleTask ioa = do
    (action, task) <- singleTask ioa
    _ <- traceForkIO "task" action
    return task

taskNotify :: Task a -> (a -> IO ()) -> IO ()
taskNotify task call = do
    _ <-
        traceForkIO "notify" $ do
            a <- taskWait task
            call a
    return ()

quickTask :: IO a -> Task a
quickTask ioa = let
    taskWait = ioa
    taskIsDone = do
        a <- ioa
        return $ Just a
    in MkTask {..}
