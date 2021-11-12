module Control.Monad.Trans.Unlift where

import Control.Monad.Trans.Constraint
import Control.Monad.Trans.Function
import Control.Monad.Trans.Tunnel
import Data.Functor.One
import Import

class ( TransTunnel t
      , TransConstraint MonadFail t
      , TransConstraint MonadIO t
      , TransConstraint MonadFix t
      , TransConstraint Monad t
      , FunctorExtract (Tunnel t)
      ) => MonadTransUnlift t where
    -- | lift with a 'WMFunction' that accounts for the transformer's effects (using MVars where necessary)
    liftWithUnlift ::
           forall m r. MonadIO m
        => (UnliftT MonadTunnelIO t -> m r)
        -> t m r
    -- | return a 'WUnliftT' that discards the transformer's effects (such as state change or output)
    getDiscardingUnlift ::
           forall m. Monad m
        => t m (WUnliftT MonadTunnelIO t)
    getDiscardingUnlift = tunnel $ \unlift -> pure $ fpure $ MkWUnliftT $ \tma -> fmap fextract $ unlift tma

discardingRunner ::
       forall t. MonadTransUnlift t
    => UnliftT MonadUnliftIO t
    -> UnliftT MonadUnliftIO t
discardingRunner run tmr = do
    MkWUnliftT du <- run getDiscardingUnlift
    du tmr

discardingWRunner ::
       forall t. MonadTransUnlift t
    => WUnliftT MonadUnliftIO t
    -> WUnliftT MonadUnliftIO t
discardingWRunner (MkWUnliftT u) = MkWUnliftT $ discardingRunner u

liftWithUnliftW ::
       forall t m. (MonadTransUnlift t, MonadTunnelIO m)
    => WMBackFunction m (t m)
liftWithUnliftW = MkWMBackFunction liftWithUnlift

readerTUnliftAllToT ::
       forall t m. (MonadTransUnlift t, MonadTunnelIO m)
    => MFunction (ReaderT (WUnliftT MonadTunnelIO t) m) (t m)
readerTUnliftAllToT rma = liftWithUnlift $ \tr -> runReaderT rma $ MkWUnliftT tr

tToReaderTUnliftAll :: MonadTunnelIO m => MFunction (t m) (ReaderT (WUnliftT Monad t) m)
tToReaderTUnliftAll tma = do
    MkWUnliftT unlift <- ask
    lift $ unlift tma

composeUnliftAllFunction ::
       (MonadTransUnlift t, MonadUnliftIO m) => UnliftT Functor t -> MFunction m n -> MFunction (t m) n
composeUnliftAllFunction rt rm tma = rm $ rt tma

composeUnliftAllFunctionCommute ::
       (MonadTransUnlift t, MonadUnliftIO m, MonadUnliftIO n) => UnliftT Functor t -> MFunction m n -> MFunction (t m) n
composeUnliftAllFunctionCommute rt rm tma = rt $ hoist rm tma

class (MonadFail m, MonadIO m, MonadFix m, MonadTunnelIO m, FunctorExtract (TunnelIO m)) => MonadUnliftIO m where
    -- | lift with an 'WIOFunction' that accounts for all transformer effects
    liftIOWithUnlift :: forall r. (MFunction m IO -> IO r) -> m r
    getDiscardingIOUnlift :: m (WIOFunction m)
    getDiscardingIOUnlift = tunnelIO $ \unlift -> pure $ fpure $ MkWMFunction $ \mr -> fmap fextract $ unlift mr

ioWMBackFunction :: MonadUnliftIO m => WMBackFunction IO m
ioWMBackFunction = MkWMBackFunction liftIOWithUnlift

instance MonadUnliftIO IO where
    liftIOWithUnlift call = call id

instance (MonadTransUnlift t, MonadUnliftIO m, MonadFail (t m), MonadIO (t m), MonadFix (t m)) => MonadUnliftIO (t m) where
    liftIOWithUnlift call = liftWithUnlift $ \tmama -> liftIOWithUnlift $ \maioa -> call $ maioa . tmama

instance MonadTransUnlift t => TransConstraint MonadUnliftIO t where
    hasTransConstraint =
        withTransConstraintDict @MonadFail $ withTransConstraintDict @MonadIO $ withTransConstraintDict @MonadFix $ Dict

instance MonadTransUnlift IdentityT where
    liftWithUnlift call = IdentityT $ call runIdentityT

instance MonadTransUnlift (ReaderT s) where
    liftWithUnlift call = ReaderT $ \s -> call $ \(ReaderT smr) -> smr s

instance Monoid s => MonadTransUnlift (WriterT s) where
    liftWithUnlift call = do
        var <- liftIO $ newMVar mempty
        r <-
            lift $
            call $ \(WriterT mrs) -> do
                (r, output) <- mrs
                liftIO $ modifyMVar_ var $ \oldoutput -> return $ mappend oldoutput output
                return r
        totaloutput <- liftIO $ takeMVar var
        tell totaloutput
        return r

instance MonadTransUnlift (StateT s) where
    liftWithUnlift call = liftWithMVarStateT $ \var -> call $ mVarRun var
