module Control.Lock.SingleThread (SingleThreadLock, mkSingleThreadLock, newSingleThreadLock, runUnlockedIOAllowAsync) where

import Control.AsyncRunner
import Control.AsyncRunner.ThreadBound
import Control.Concurrent.TPieceVar
import Control.Lock.IsLock
import Shapes.Import

data SingleThreadLock = MkSingleTheadLock
    { stlVar :: TPieceVar (IO ())
    , stlIsThread :: IO Bool
    }

instance IsLock SingleThreadLock where
    runLockedIO lock la = do
        isThread <- stlIsThread lock
        if isThread
            then la
            else pusherWait (\work -> atomically $ ppPush (stlVar lock) work) la
    runUnlockedIO _ = id

isThisThread :: ThreadId -> IO Bool
isThisThread threadId = do
    thisThread <- myThreadId
    return $ threadId == thisThread

mkSingleThreadLock :: TPieceVar (IO ()) -> IO Bool -> SingleThreadLock
mkSingleThreadLock = MkSingleTheadLock

newSingleThreadLock :: TPieceVar (IO ()) -> IO SingleThreadLock
newSingleThreadLock var = do
    thisThread <- myThreadId
    return $ MkSingleTheadLock var $ isThisThread thisThread

runUnlockedIOAllowAsync :: SingleThreadLock -> IO --> IO
runUnlockedIOAllowAsync lock = asyncIORunnerThreadBound $ stlVar lock
