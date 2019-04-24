module Truth.Core.Object.UnliftIO where

import Truth.Core.Import
import Truth.Debug

data CloseUnliftIO f (a :: k) =
    forall m. MonadStackIO m =>
                  MkCloseUnliftIO (UnliftIO m)
                                  (f m a)

lensObjectUnliftFull ::
       forall t m. (MonadTransUnlift t, MonadUnliftIO m, MonadIO (t m))
    => Unlift t
    -> UnliftIO m
    -> UnliftIO (t m)
lensObjectUnliftFull (MkUnlift lensRun) (MkTransform objRunA) =
    MkTransform $ \tmr -> objRunA $ traceBarrier "lensObject.run.full" lensRun $ liftWithUnlift $ \(MkUnlift unlift) -> unlift tmr

lensObjectUnliftDiscard ::
       forall t m. (MonadTransUnlift t, MonadUnliftIO m, MonadIO (t m))
    => Unlift t
    -> UnliftIO m
    -> UnliftIO (t m)
lensObjectUnliftDiscard (MkUnlift lensRun) (MkTransform objRunA) =
    MkTransform $ \tmr ->
        objRunA $ do
            MkUnlift du <- traceBarrier "lensObject.run.discard" lensRun $ getDiscardingUnlift
            traceBracket "lensObject.run.discard.unlift" $ du tmr -- discard lens effects: all these effects will be replayed by the update

lensObjectUnlift ::
       forall t m. (MonadTransUnlift t, MonadUnliftIO m, MonadIO (t m))
    => Bool
    -> Unlift t
    -> UnliftIO m
    -> UnliftIO (t m)
lensObjectUnlift False = lensObjectUnliftFull
lensObjectUnlift True = lensObjectUnliftDiscard
