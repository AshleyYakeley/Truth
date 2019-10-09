module Truth.Core.Object.Run where

import Truth.Core.Import

data RunnableIO f (a :: k) =
    forall m. MonadStackIO m =>
                  MkRunnableIO (WIOFunction m)
                               (f m a)

lensObjectUnliftFull ::
       forall t m. (MonadTransUntrans t, MonadUnliftIO m)
    => WUntransFunction t
    -> WIOFunction m
    -> WIOFunction (t m)
lensObjectUnliftFull (MkWUntransFunction lensRun) (MkWMFunction objRunA) =
    MkWMFunction $ \tmr -> objRunA $ lensRun $ liftWithUntrans $ \unlift -> unlift tmr

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
