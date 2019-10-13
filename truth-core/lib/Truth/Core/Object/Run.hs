module Truth.Core.Object.Run where

import Truth.Core.Import

data RunnableIO f (a :: k) =
    forall m. MonadUnliftIOStack m =>
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

class RunnableIOMap (f :: (Type -> Type) -> Type -> Type) where
    ioMapRunnable ::
           forall m1 m2 a. (MonadUnliftIOStack m1, MonadUnliftIOStack m2)
        => MBackFunction m1 m2
        -> f m1 a
        -> f m2 a

joinRunnableIOs ::
       (RunnableIOMap f1, RunnableIOMap f2)
    => (forall m. MonadUnliftIOStack m => f1 m a1 -> f2 m a2 -> f3 m a3)
    -> RunnableIO f1 a1
    -> RunnableIO f2 a2
    -> RunnableIO f3 a3
joinRunnableIOs ff (MkRunnableIO (run1 :: IOFunction m1) fma1) (MkRunnableIO (run2 :: IOFunction m2) fma2) =
    case isCombineMonadUnliftIOStack @m1 @m2 of
        Dict ->
            MkRunnableIO (combineUnliftIOFunctions run1 run2) $
            ff
                @(CombineIOStack m1 m2)
                (ioMapRunnable (combineUnliftFstMBackFunction @m1 @m2) fma1)
                (ioMapRunnable (combineUnliftSndMBackFunction @m1 @m2) fma2)
