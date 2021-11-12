module Control.Monad.Ology.Trans.ReaderState where

import Control.Monad.Ology.Function
import Import

type ReaderStateT f m = StateT (WMFunction f m) m

evalReaderStateT :: Monad m => ReaderStateT f m a -> (forall t. f t -> m t) -> m a
evalReaderStateT rsa fm = evalStateT rsa (MkWMFunction fm)

liftRS :: (Monad f, Monad m) => f a -> ReaderStateT f m a
liftRS fa = do
    MkWMFunction fm <- get
    a <- lift $ fm fa
    put $ MkWMFunction $ \c -> fm $ fa >> c
    return a

updateRS :: Monad m => (forall a. f a -> f a) -> ReaderStateT f m ()
updateRS ff = modify (\fm -> fm . MkWMFunction ff)
