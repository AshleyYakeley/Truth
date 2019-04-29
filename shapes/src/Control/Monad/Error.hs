module Control.Monad.Error where

import Shapes.Import

class Monad m => MonadError e m where
    throwError :: e -> m a

instance MonadError () Maybe where
    throwError () = Nothing

instance MonadError () [] where
    throwError () = []

instance (MonadTrans t, MonadError e m, Monad (t m)) => MonadError e (t m) where
    throwError e = lift $ throwError e
