module Control.AsyncRunner.ThreadBound
    ( asyncIORunnerThreadBound
    )
where

import Control.AsyncRunner
import Shapes.Import

data QState a = WorkQState (IO ()) | DoneQState (Result (Exc IO) a)

-- | Runs all jobs in the same thread. Use this when you care about thread identity/affinity.
asyncIORunnerThreadBound :: forall r. (Pusher -> IO r) -> IO r
asyncIORunnerThreadBound call = do
    var <- newEmptyMVar
    let
        pusher :: Pusher
        pusher doit = putMVar var $ WorkQState doit
    _ <- forkOS $ do
        ra <- tryExc $ call pusher
        putMVar var $ DoneQState ra
    let
        go :: IO r
        go = do
            qs <- takeMVar var
            case qs of
                WorkQState doit -> do
                    doit
                    go
                DoneQState ra -> fromResultExc ra
    go
