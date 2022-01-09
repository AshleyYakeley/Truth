module Control.Monad.Ology.Trans.Unlift where

import Control.Monad.Ology.Function
import Control.Monad.Ology.Functor.One
import Control.Monad.Ology.Trans.Constraint
import Control.Monad.Ology.Trans.Tunnel
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
        => (Unlift MonadTunnelIO t -> m r)
        -> t m r
    -- | return a 'WUnlift' that discards the transformer's effects (such as state change or output)
    getDiscardingUnlift ::
           forall m. Monad m
        => t m (WUnlift MonadTunnelIO t)
    getDiscardingUnlift = tunnel $ \unlift -> pure $ fpure $ MkWUnlift $ \tma -> fmap fextract $ unlift tma

discardingRunner ::
       forall t. MonadTransUnlift t
    => Unlift MonadUnliftIO t
    -> Unlift MonadUnliftIO t
discardingRunner run tmr = do
    MkWUnlift du <- run getDiscardingUnlift
    du tmr

discardingWRunner ::
       forall t. MonadTransUnlift t
    => WUnlift MonadUnliftIO t
    -> WUnlift MonadUnliftIO t
discardingWRunner (MkWUnlift u) = MkWUnlift $ discardingRunner u

liftWithUnliftW ::
       forall t m. (MonadTransUnlift t, MonadTunnelIO m)
    => WMBackFunction m (t m)
liftWithUnliftW = MkWMBackFunction liftWithUnlift

readerTUnliftAllToT ::
       forall t m. (MonadTransUnlift t, MonadTunnelIO m)
    => ReaderT (WUnlift MonadTunnelIO t) m --> t m
readerTUnliftAllToT rma = liftWithUnlift $ \tr -> runReaderT rma $ MkWUnlift tr

tToReaderTUnliftAll :: MonadTunnelIO m => t m --> ReaderT (WUnlift Monad t) m
tToReaderTUnliftAll tma = do
    MkWUnlift unlift <- ask
    lift $ unlift tma

composeUnliftAllFunction :: (MonadTransUnlift t, MonadUnliftIO m) => Unlift Functor t -> (m --> n) -> (t m --> n)
composeUnliftAllFunction rt rm tma = rm $ rt tma

composeUnliftAllFunctionCommute ::
       (MonadTransUnlift t, MonadUnliftIO m, MonadUnliftIO n) => Unlift Functor t -> (m --> n) -> (t m --> n)
composeUnliftAllFunctionCommute rt rm tma = rt $ hoist rm tma

class (MonadFail m, MonadIO m, MonadFix m, MonadTunnelIO m, FunctorExtract (TunnelIO m)) => MonadUnliftIO m where
    -- | lift with an unlift that accounts for all transformer effects
    liftIOWithUnlift :: forall r. ((m --> IO) -> IO r) -> m r
    getDiscardingIOUnlift :: m (WMFunction m IO)
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
