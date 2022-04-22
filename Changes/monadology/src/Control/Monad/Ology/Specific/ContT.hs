{-# OPTIONS -fno-warn-orphans #-}

module Control.Monad.Ology.Specific.ContT
    ( module Control.Monad.Trans.Cont
    , module Control.Monad.Ology.Specific.ContT
    ) where

import Control.Monad.Ology.General
import Control.Monad.Ology.Specific.ReaderT
import Control.Monad.Ology.Specific.StateT
import Control.Monad.Trans.Cont hiding (callCC)
import qualified Control.Monad.Trans.Cont as T
import Import

instance TransConstraint Functor (ContT s) where
    hasTransConstraint = Dict

instance TransConstraint Applicative (ContT s) where
    hasTransConstraint = Dict

instance TransConstraint Monad (ContT s) where
    hasTransConstraint = Dict

instance TransConstraint MonadIO (ContT s) where
    hasTransConstraint = Dict

instance TransConstraint MonadFail (ContT s) where
    hasTransConstraint = Dict

instance forall k (r :: k) (m :: k -> Type). MonadCont (ContT r m) where
    callCC = T.callCC

instance MonadTransCoerce (ContT r) where
    transCoerce = Dict

updateContT :: (m r -> m r) -> ContT r m ()
updateContT m = ContT $ \umr -> m $ umr ()

stateToReaderContT :: Monad m => StateT s m a -> ContT r (ReaderT s m) a
stateToReaderContT (StateT sma) =
    ContT $ \c ->
        ReaderT $ \olds -> do
            (a, news) <- sma olds
            runReaderT (c a) news

hoistContT :: (m1 r1 -> m2 r2) -> (m2 r2 -> m1 r1) -> ContT r1 m1 a -> ContT r2 m2 a
hoistContT m12 m21 (ContT amrmr) = ContT $ \c -> m12 $ amrmr (m21 . c)
