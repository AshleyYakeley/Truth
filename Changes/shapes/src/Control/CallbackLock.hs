module Control.CallbackLock
    ( CallbackLock
    , newCallbackLock
    , cbRunLocked
    , cbRunUnlocked
    ) where

import Shapes.Import

newtype CallbackLock =
    MkCallbackLock (TVar (Int, Bool))

newCallbackLock :: IO CallbackLock
newCallbackLock = do
    var <- newTVarIO (0, False)
    return $ MkCallbackLock var

cbRunLocked :: CallbackLock -> IO --> IO
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

cbRunUnlocked :: CallbackLock -> IO --> IO
cbRunUnlocked (MkCallbackLock var) = let
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
    in \call -> bracket before after $ \_ -> call
