module Control.Monad.MonadCont where

import Control.Monad.Trans.Cont as T
import Shapes.Import

class Monad m => MonadCont m where
    callCC :: ((a -> m b) -> m a) -> m a

instance forall k (r :: k) (m :: k -> Type). MonadCont (ContT r m) where
    callCC = T.callCC
