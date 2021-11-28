module Control.Monad.Ology.Trans.ContExtra where

import Import

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
