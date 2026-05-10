module Control.Lock.SingleThread (SingleThreadLock, newSingleThreadLock) where

import Control.Lock.IsLock
import Shapes.Import

data family SingleThreadLock (ls :: LockState) :: Type

data instance SingleThreadLock 'Locked = MkLockedSingleTheadLock (IO --> IO)

data instance SingleThreadLock 'Unlocked = MkUnlockedSingleTheadLock (IO --> IO) ThreadId

instance IsLock SingleThreadLock where
    runLockedIO (MkUnlockedSingleTheadLock runInThread threadId) la = do
        thisThread <- myThreadId
        if threadId == thisThread
            then la $ MkLockedSingleTheadLock runInThread
            else runInThread $ la $ MkLockedSingleTheadLock runInThread
    runUnlockedIO lock ua = do
        lock' <- provideUnlockedIO lock
        ua lock'
    provideUnlockedIO (MkLockedSingleTheadLock runInThread) = do
        thisThread <- myThreadId
        return $ MkUnlockedSingleTheadLock runInThread thisThread

newSingleThreadLock :: (IO --> IO) -> IO (SingleThreadLock 'Unlocked)
newSingleThreadLock runInThread = do
    thisThread <- myThreadId
    return $ MkUnlockedSingleTheadLock runInThread thisThread
