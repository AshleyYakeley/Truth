module Truth.Core.Reference.AutoClose where

import Truth.Core.Import

type AutoCloseT key t = StateT (StrictMap key (t, LifeState IO))

runAutoClose :: Ord key => UnliftAll MonadUnliftIO (AutoCloseT key t)
runAutoClose ac = do
    (a, mp) <- runStateT ac mempty
    liftIO $ for_ (toList mp) $ closeLifeState . snd
    return a

acOpenReference :: Ord key => key -> With IO t -> AutoCloseT key t IO t
acOpenReference key withX = do
    oldmap <- get
    case lookup key oldmap of
        Just mutedcloser -> return $ fst mutedcloser
        Nothing -> do
            mutedcloser <- lift $ getLifeState $ lifeCycleWith withX
            put $ insertMap key mutedcloser oldmap
            return $ fst mutedcloser
