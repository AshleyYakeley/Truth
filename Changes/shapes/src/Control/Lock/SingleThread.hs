module Control.Lock.SingleThread (SingleThreadLock, mkSingleThreadLock, newSingleThreadLock, runUnlockedIOAllowAsync) where

import Control.AsyncRunner
import Control.AsyncRunner.ThreadBound
import Control.Concurrent.TPieceVar
import Control.Lock.IsLock
import Shapes.Import

data family SingleThreadLock (ls :: LockState) :: Type

data instance SingleThreadLock 'Locked = MkLockedSingleTheadLock (TPieceVar (IO ()))

data instance SingleThreadLock 'Unlocked = MkUnlockedSingleTheadLock (TPieceVar (IO ())) ThreadId

instance IsLock SingleThreadLock where
    runLockedIO (MkUnlockedSingleTheadLock var threadId) la = do
        thisThread <- myThreadId
        if threadId == thisThread
            then la $ MkLockedSingleTheadLock var
            else pusherWait (\work -> atomically $ ppPush var work) $ la $ MkLockedSingleTheadLock var
    runUnlockedIO llock ua = do
        ulock <- provideUnlockedIO llock
        ua ulock
    provideUnlockedIO (MkLockedSingleTheadLock var) = do
        thisThread <- myThreadId
        return $ MkUnlockedSingleTheadLock var thisThread

mkSingleThreadLock :: ThreadId -> TPieceVar (IO ()) -> SingleThreadLock 'Unlocked
mkSingleThreadLock threadId var = MkUnlockedSingleTheadLock var threadId

newSingleThreadLock :: TPieceVar (IO ()) -> IO (SingleThreadLock 'Unlocked)
newSingleThreadLock var = do
    thisThread <- myThreadId
    return $ MkUnlockedSingleTheadLock var thisThread

runUnlockedIOAllowAsync :: forall a. SingleThreadLock 'Locked -> (SingleThreadLock 'Unlocked -> IO a) -> IO a
runUnlockedIOAllowAsync llock@(MkLockedSingleTheadLock var) call = do
    ulock <- provideUnlockedIO llock
    asyncIORunnerThreadBound var $ call ulock
