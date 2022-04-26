module Control.Monad.Ology.General.Trans.Unlift where

import Control.Monad.Ology.General.Extract
import Control.Monad.Ology.General.Function
import Control.Monad.Ology.General.IO
import Control.Monad.Ology.General.Outer
import Control.Monad.Ology.General.Trans.Constraint
import Control.Monad.Ology.General.Trans.Hoist
import Control.Monad.Ology.General.Trans.Tunnel
import Control.Monad.Ology.Specific.ComposeOuter
import Import

class ( MonadTransTunnel t
      , TransConstraint MonadFail t
      , TransConstraint MonadIO t
      , TransConstraint MonadFix t
      , TransConstraint Monad t
      , MonadExtract (Tunnel t)
      ) => MonadTransUnlift t where
    -- | lift with a 'WMFunction' that accounts for the transformer's effects (using MVars where necessary)
    liftWithUnlift ::
           forall m r. MonadIO m
        => ((forall m'. MonadTunnelIOInner m' => t m' --> m') -> m r)
        -> t m r
    -- | return a 'WUnlift' that discards the transformer's effects (such as state change or output)
    getDiscardingUnlift ::
           forall m. Monad m
        => t m (WUnlift MonadTunnelIOInner t)
    getDiscardingUnlift = tunnel $ \unlift -> pure $ pure $ MkWUnlift $ \tma -> fmap mToValue $ unlift tma

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
       forall t m. (MonadTransUnlift t, MonadTunnelIOInner m)
    => WMBackFunction m (t m)
liftWithUnliftW = MkWMBackFunction $ \call -> liftWithUnlift $ \unlift -> call unlift

composeUnliftAllFunction :: (MonadTransUnlift t, MonadUnliftIO m) => Unlift Functor t -> (m --> n) -> (t m --> n)
composeUnliftAllFunction rt rm tma = rm $ rt tma

composeUnliftAllFunctionCommute ::
       (MonadTransUnlift t, MonadUnliftIO m, MonadUnliftIO n) => Unlift Functor t -> (m --> n) -> (t m --> n)
composeUnliftAllFunctionCommute rt rm tma = rt $ hoist rm tma

class (MonadFail m, MonadIO m, MonadFix m, MonadTunnelIO m, MonadExtract (TunnelIO m)) => MonadUnliftIO m where
    -- | lift with an unlift that accounts for all transformer effects
    liftIOWithUnlift :: IO -/-> m
    getDiscardingIOUnlift :: m (WMFunction m IO)
    getDiscardingIOUnlift = tunnelIO $ \unlift -> pure $ pure $ MkWMFunction $ \mr -> fmap mToValue $ unlift mr

ioWMBackFunction :: MonadUnliftIO m => WMBackFunction IO m
ioWMBackFunction = MkWMBackFunction liftIOWithUnlift

instance MonadUnliftIO IO where
    liftIOWithUnlift call = call id

instance (MonadTransUnlift t, MonadUnliftIO m, MonadFail (t m), MonadIO (t m), MonadFix (t m)) => MonadUnliftIO (t m) where
    liftIOWithUnlift call = liftWithUnlift $ \tmama -> liftIOWithUnlift $ \maioa -> call $ maioa . tmama

instance MonadTransUnlift t => TransConstraint MonadUnliftIO t where
    hasTransConstraint =
        withTransConstraintDict @MonadFail $ withTransConstraintDict @MonadIO $ withTransConstraintDict @MonadFix $ Dict

instance MonadOuter outer => MonadTransUnlift (ComposeOuter outer) where
    liftWithUnlift call =
        MkComposeOuter $ do
            MkExtract extract <- getExtract
            return $ call $ extract . getComposeOuter
