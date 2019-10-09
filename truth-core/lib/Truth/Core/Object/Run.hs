module Truth.Core.Object.Run where

import Truth.Core.Import

data RunnableIO f (a :: k) =
    forall m. MonadStackIO m =>
                  MkRunnableIO (IOFunction m)
                               (f m a)

lensObjectUnliftFull ::
       forall t m. (MonadTransUntrans t, MonadUnliftIO m)
    => Untrans t
    -> IOFunction m
    -> IOFunction (t m)
lensObjectUnliftFull lensRun objRunA tmr = objRunA $ lensRun $ liftWithUntrans $ \unlift -> unlift tmr

lensObjectUnliftDiscard ::
       forall t m. (MonadTransUntrans t, MonadUnliftIO m)
    => Untrans t
    -> IOFunction m
    -> IOFunction (t m)
lensObjectUnliftDiscard lensRun objRunA tmr =
    objRunA $ do
        MkWUntrans du <- lensRun $ getDiscardingUntrans
        du tmr -- discard lens effects: all these effects will be replayed by the update

lensObjectUnlift ::
       forall t m. (MonadTransUntrans t, MonadUnliftIO m)
    => Bool
    -> Untrans t
    -> IOFunction m
    -> IOFunction (t m)
lensObjectUnlift False = lensObjectUnliftFull
lensObjectUnlift True = lensObjectUnliftDiscard
