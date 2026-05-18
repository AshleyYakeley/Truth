module Control.Lock.SingleThread (SingleThreadLock, newSingleThreadLock, runUnlockedIOAllowAsync) where

import Control.AsyncRunner
import Control.AsyncRunner.ThreadBound
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

runUnlockedIOAllowAsync :: forall a. SingleThreadLock 'Locked -> (SingleThreadLock 'Unlocked -> IO a) -> IO a
runUnlockedIOAllowAsync (MkLockedSingleTheadLock _) call = do
    thisThread <- myThreadId
    asyncIORunnerThreadBound $ \pusher ->
        let
            runInThread :: IO --> IO
            runInThread = pusherWait pusher

            newLock :: SingleThreadLock 'Unlocked
            newLock = MkUnlockedSingleTheadLock runInThread thisThread
            in call newLock
