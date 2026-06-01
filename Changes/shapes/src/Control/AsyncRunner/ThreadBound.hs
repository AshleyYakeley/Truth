module Control.AsyncRunner.ThreadBound
    ( pipe
    , asyncIORunnerThreadBound
    )
where

import Control.AsyncRunner
import Control.Concurrent.TPieceVar
import Shapes.Import

pipe :: forall final item. IO (Either item final) -> (item -> IO ()) -> IO final
pipe pull push = do
    let
        go :: IO final
        go = do
            eif <- pull
            case eif of
                Left i -> do
                    push i
                    go
                Right f -> return f
    go

-- | Runs all jobs in the same thread. Use this when you care about thread identity/affinity.
asyncIORunnerThreadBound :: forall r. TPieceVar (IO ()) -> IO r -> IO r
asyncIORunnerThreadBound workVar call = do
    doneVar <- atomically mkWholeTPieceVar
    forkOSPusher $ do
        ra <- tryExc call
        atomically $ ppPush doneVar ra
    ra <- pipe (atomically $ ppPull $ workVar <+++> doneVar) id
    fromResultExc ra
