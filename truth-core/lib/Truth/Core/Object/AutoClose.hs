module Truth.Core.Object.AutoClose where

import Truth.Core.Import

type AutoClose key t = StateT (StrictMap key (t, IO ())) IO

runAutoClose :: Ord key => WIOFunction (AutoClose key t)
runAutoClose =
    MkWMFunction $ \ac -> do
        (a, mp) <- runStateT ac mempty
        for_ (toList mp) snd
        return a

acOpenObject :: Ord key => key -> With IO t -> AutoClose key t t
acOpenObject key withX = do
    oldmap <- get
    case lookup key oldmap of
        Just mutedcloser -> return $ fst mutedcloser
        Nothing -> do
            mutedcloser <- lift $ getLifeState $ lifeCycleWith withX
            put $ insertMap key mutedcloser oldmap
            return $ fst mutedcloser
