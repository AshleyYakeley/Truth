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

liftRS :: Monad m => f a -> ReaderStateT f m a
liftRS fa = do
    MkTransform fm <- get
    lift $ fm fa

updateRS :: Monad m => (forall a. f a -> f a) -> ReaderStateT f m ()
updateRS ff = modify (\fm -> fm . MkTransform ff)
