module Control.Monad.Trans.ReaderState where

import Control.Category
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Transform
import Prelude hiding ((.), id)

type ReaderStateT f m = StateT (Transform f m) m

evalReaderStateT :: Monad m => ReaderStateT f m a -> (forall t. f t -> m t) -> m a
evalReaderStateT rsa fm = evalStateT rsa (MkTransform fm)

liftRS :: (Monad f, Monad m) => f a -> ReaderStateT f m a
liftRS fa = do
    MkTransform fm <- get
    a <- lift $ fm fa
    put $ MkTransform $ \cont -> fm $ fa >> cont
    return a

updateRS :: Monad m => (forall a. f a -> f a) -> ReaderStateT f m ()
updateRS ff = modify (\fm -> fm . MkTransform ff)
