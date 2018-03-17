module Truth.Core.Object.AutoClose where

import Data.Map.Strict hiding (lookup)
import Truth.Core.Import

type AutoClose key t = StateT (Map key (t, IO ())) IO

runAutoClose :: Ord key => UnliftIO (AutoClose key t)
runAutoClose =
    MkUnliftIO $ \ac -> do
        (a, mp) <- runStateT ac mempty
        for_ (elems mp) snd
        return a

acOpenObject :: Ord key => key -> With t -> AutoClose key t t
acOpenObject key withX = do
    oldmap <- get
    case lookup key oldmap of
        Just mutedcloser -> return $ fst mutedcloser
        Nothing -> do
            mutedcloser <- lift $ runLifeCycle $ lifeCycleWith withX
            put $ insert key mutedcloser oldmap
            return $ fst mutedcloser
