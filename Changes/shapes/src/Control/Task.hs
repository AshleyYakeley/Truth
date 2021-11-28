module Control.Task where

import Control.Monad.Ology.Compose
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
    (MkTask wab dab) <*> (MkTask wa da) = MkTask (wab <*> wa) (getComposeM $ (MkComposeM dab) <*> (MkComposeM da))

instance Semigroup (Task ()) where
    p <> q = p *> q

instance Monoid (Task ()) where
    mempty = pure ()

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

singleTask :: IO a -> IO (IO (), Task a)
singleTask ioa = do
    var <- newEmptyMVar
    let
        action :: IO ()
        action = do
            a <- ioa
            putMVar var a
        task = MkTask (takeMVar var) (tryReadMVar var)
    return (action, task)

forkSingleTask :: IO a -> IO (Task a)
forkSingleTask ioa = do
    (action, task) <- singleTask ioa
    _ <- forkIO $ traceBracketIO "THREAD: task" action
    return task

taskNotify :: Task a -> (a -> IO ()) -> IO ()
taskNotify task call = do
    _ <-
        forkIO $ traceBracketIO "THREAD: notify" $ do
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
