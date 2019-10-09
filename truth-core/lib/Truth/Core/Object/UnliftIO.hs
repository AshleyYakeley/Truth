module Truth.Core.Object.UnliftIO where

import Truth.Core.Import

data CloseUnliftIO f (a :: k) =
    forall m. MonadStackIO m =>
                  MkCloseUnliftIO (WIOFunction m)
                                  (f m a)

lensObjectUnliftFull ::
       forall t m. (MonadTransUntrans t, MonadUnliftIO m)
    => WUntransFunction t
    -> WIOFunction m
    -> WIOFunction (t m)
lensObjectUnliftFull (MkWUntransFunction lensRun) (MkWMFunction objRunA) =
    MkWMFunction $ \tmr -> objRunA $ lensRun $ liftWithUntrans $ \(MkWUntransFunction unlift) -> unlift tmr

lensObjectUnliftDiscard ::
       forall t m. (MonadTransUntrans t, MonadUnliftIO m)
    => WUntransFunction t
    -> WIOFunction m
    -> WIOFunction (t m)
lensObjectUnliftDiscard (MkWUntransFunction lensRun) (MkWMFunction objRunA) =
    MkWMFunction $ \tmr ->
        objRunA $ do
            MkWUntransFunction du <- lensRun $ getDiscardingUntrans
            du tmr -- discard lens effects: all these effects will be replayed by the update

lensObjectUnlift ::
       forall t m. (MonadTransUntrans t, MonadUnliftIO m)
    => Bool
    -> WUntransFunction t
    -> WIOFunction m
    -> WIOFunction (t m)
lensObjectUnlift False = lensObjectUnliftFull
lensObjectUnlift True = lensObjectUnliftDiscard
