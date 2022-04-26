{-# OPTIONS -fno-warn-orphans #-}

module Control.Monad.Ology.Specific.ReaderT
    ( module Control.Monad.Trans.Reader
    , module Control.Monad.Ology.Specific.ReaderT
    ) where

import Control.Monad.Ology.General
import Control.Monad.Trans.Reader hiding (liftCallCC, liftCatch)
import Import

instance TransConstraint Functor (ReaderT s) where
    hasTransConstraint = Dict

instance TransConstraint Applicative (ReaderT s) where
    hasTransConstraint = Dict

instance TransConstraint Monad (ReaderT s) where
    hasTransConstraint = Dict

instance TransConstraint MonadIO (ReaderT s) where
    hasTransConstraint = Dict

instance TransConstraint MonadFail (ReaderT s) where
    hasTransConstraint = Dict

instance TransConstraint MonadFix (ReaderT s) where
    hasTransConstraint = Dict

instance TransConstraint MonadPlus (ReaderT s) where
    hasTransConstraint = Dict

instance MonadTransCoerce (ReaderT r) where
    transCoerce = Dict

instance MonadException m => MonadException (ReaderT r m) where
    type Exc (ReaderT r m) = Exc m
    throwExc e = lift $ throwExc e
    catchExc tma handler = tunnel $ \unlift -> catchExc (unlift tma) $ \e -> unlift $ handler e

instance TransConstraint MonadException (ReaderT r) where
    hasTransConstraint = Dict

instance MonadThrow e m => MonadThrow e (ReaderT r m) where
    throw e = lift $ throw e

instance TransConstraint (MonadThrow e) (ReaderT r) where
    hasTransConstraint = Dict

instance MonadCatch e m => MonadCatch e (ReaderT r m) where
    catch ma handler = tunnel $ \unlift -> catch (unlift ma) $ \e -> unlift $ handler e

instance TransConstraint (MonadCatch e) (ReaderT r) where
    hasTransConstraint = Dict

instance MonadOuter m => MonadOuter (ReaderT r m) where
    getExtract =
        ReaderT $ \r -> do
            MkExtract maa <- getExtract
            return $ MkExtract $ \(ReaderT rma) -> maa $ rma r

instance TransConstraint MonadOuter (ReaderT s) where
    hasTransConstraint = Dict

instance MonadTransHoist (ReaderT r) where
    hoist = tunnelHoist

instance MonadTransTunnel (ReaderT r) where
    type Tunnel (ReaderT r) = Identity
    tunnel call = ReaderT $ \r -> fmap runIdentity $ call $ \(ReaderT smr) -> fmap Identity $ smr r

instance MonadTransUnlift (ReaderT r) where
    liftWithUnlift call = ReaderT $ \r -> call $ \(ReaderT smr) -> smr r

instance MonadTransAskUnlift (ReaderT r)

readerTUnliftAllToT ::
       forall t m. (MonadTransUnlift t, MonadTunnelIO m)
    => ReaderT (WUnlift MonadTunnelIOInner t) m --> t m
readerTUnliftAllToT rma = liftWithUnlift $ \tr -> runReaderT rma $ MkWUnlift tr

tToReaderTUnliftAll :: MonadTunnelIO m => t m --> ReaderT (WUnlift Monad t) m
tToReaderTUnliftAll tma = do
    MkWUnlift unlift <- ask
    lift $ unlift tma
