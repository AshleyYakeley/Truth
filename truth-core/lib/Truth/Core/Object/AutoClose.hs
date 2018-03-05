module Truth.Core.Object.AutoClose where

import Data.Map.Strict hiding (lookup)
import Truth.Core.Import

newtype OpenClose t = MkOpenClose
    { unOpenClose :: IO (t, IO ())
    }

instance Functor OpenClose where
    fmap ab (MkOpenClose oc) =
        MkOpenClose $ do
            (a, closer) <- oc
            return (ab a, closer)

instance Applicative OpenClose where
    pure t = MkOpenClose $ return (t, return ())
    (MkOpenClose ocab) <*> (MkOpenClose oca) =
        MkOpenClose $ do
            (ab, clab) <- ocab
            (a, cla) <- oca
            return (ab a, cla >> clab)

type With t = forall r. (t -> IO r) -> IO r

openToWith :: OpenClose t -> With t
openToWith (MkOpenClose oc) run = do
    (t, closer) <- oc
    finally (run t) closer

withToOpen :: With t -> OpenClose t
withToOpen withX =
    MkOpenClose $ do
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

acOpenObject :: Ord key => key -> With t -> AutoClose key t t
acOpenObject key withX = do
    oldmap <- get
    case lookup key oldmap of
        Just mutedcloser -> return $ fst mutedcloser
        Nothing -> do
            mutedcloser <- lift $ unOpenClose $ withToOpen withX
            put $ insert key mutedcloser oldmap
            return $ fst mutedcloser
