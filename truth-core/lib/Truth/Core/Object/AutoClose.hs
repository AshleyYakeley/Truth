module Truth.Core.Object.AutoClose where

import Data.Map.Strict hiding (lookup)
import Truth.Core.Import

withToOpen :: (forall r. (t -> IO r) -> IO r) -> IO (t, IO ())
withToOpen withX = do
    tVar <- newEmptyMVar
    closerVar <- newEmptyMVar
    doneVar <- newEmptyMVar
    _ <-
        forkIO $ do
            withX $ \t -> do
                putMVar tVar t
                takeMVar closerVar
            putMVar doneVar ()
    t <- takeMVar tVar
    let
        close :: IO ()
        close = do
            putMVar closerVar ()
            takeMVar doneVar
    return (t, close)

type AutoClose key t = StateT (Map key (t, IO ())) IO

runAutoClose :: Ord key => UnliftIO (AutoClose key t)
runAutoClose =
    MkUnliftIO $ \ac -> do
        (a, mp) <- runStateT ac mempty
        for_ (elems mp) snd
        return a

acOpenObject :: Ord key => key -> (forall r. (t -> IO r) -> IO r) -> AutoClose key t t
acOpenObject key withX = do
    oldmap <- get
    case lookup key oldmap of
        Just mutedcloser -> return $ fst mutedcloser
        Nothing -> do
            mutedcloser <- lift $ withToOpen withX
            put $ insert key mutedcloser oldmap
            return $ fst mutedcloser
