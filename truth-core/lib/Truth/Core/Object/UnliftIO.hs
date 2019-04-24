module Truth.Core.Object.UnliftIO where

import Truth.Core.Import

data CloseUnliftIO f (a :: k) =
    forall m. MonadStackIO m =>
                  MkCloseUnliftIO (UnliftIO m)
                                  (f m a)

lensObjectUnliftFull ::
       forall t m. (MonadTransUnlift t, MonadUnliftIO m)
    => Unlift t
    -> UnliftIO m
    -> UnliftIO (t m)
lensObjectUnliftFull (MkUnlift lensRun) (MkTransform objRunA) =
    MkTransform $ \tmr -> objRunA $ lensRun $ liftWithUnlift $ \(MkUnlift unlift) -> unlift tmr

lensObjectUnliftDiscard ::
       forall t m. (MonadTransUnlift t, MonadUnliftIO m)
    => Unlift t
    -> UnliftIO m
    -> UnliftIO (t m)
lensObjectUnliftDiscard (MkUnlift lensRun) (MkTransform objRunA) =
    MkTransform $ \tmr ->
        objRunA $ do
            MkUnlift du <- lensRun $ getDiscardingUnlift
            du tmr -- discard lens effects: all these effects will be replayed by the update

lensObjectUnlift ::
       forall t m. (MonadTransUnlift t, MonadUnliftIO m)
    => Bool
    -> Unlift t
    -> UnliftIO m
    -> UnliftIO (t m)
lensObjectUnlift False = lensObjectUnliftFull
lensObjectUnlift True = lensObjectUnliftDiscard
