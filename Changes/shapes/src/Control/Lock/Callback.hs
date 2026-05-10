module Control.Lock.Callback
    ( CallbackLock
    , callbackLockRelock -- REMOVE
    , newCallbackLock
    , cbRunLocked
    , cbRunUnlocked
    )
where

import Control.Lock.IsLock
import Shapes.Import

newtype CallbackLock (ls :: LockState)
    = MkCallbackLock (TVar (Int, Bool))

callbackLockRelock :: CallbackLock lsa -> CallbackLock lsb
callbackLockRelock (MkCallbackLock var) = MkCallbackLock var

newCallbackLock :: IO (CallbackLock 'Unlocked)
newCallbackLock = do
    var <- newTVarIO (0, False)
    return $ MkCallbackLock var

cbRunLocked :: CallbackLock 'Unlocked -> IO --> IO
cbRunLocked (MkCallbackLock var) = let
    before :: IO ()
    before =
        atomically $ do
            (i, lock) <- readTVar var
            if lock
                then mzero
                else writeTVar var (succ i, True)
    after :: IO ()
    after =
        atomically $ do
            (i, lock) <- readTVar var
            if lock
                then writeTVar var (pred i, False)
                else mzero
    in bracket_ before after

cbRunUnlocked :: CallbackLock 'Locked -> IO --> IO
cbRunUnlocked (MkCallbackLock var) call = let
    before :: IO Int
    before =
        atomically $ do
            (i, lock) <- readTVar var
            if lock
                then do
                    writeTVar var (i, False)
                    return i
                else mzero
    after :: Int -> IO ()
    after i' =
        atomically $ do
            ilock <- readTVar var
            case ilock of
                (i, False)
                    | i == i' -> writeTVar var (i, True)
                _ -> mzero
    in bracket before after $ \_ -> call

instance IsLock CallbackLock where
    runLockedIO lock la = cbRunLocked lock $ la $ callbackLockRelock lock
    runUnlockedIO lock ua = cbRunUnlocked lock $ ua $ callbackLockRelock lock
    provideUnlockedIO lock = return $ callbackLockRelock lock
