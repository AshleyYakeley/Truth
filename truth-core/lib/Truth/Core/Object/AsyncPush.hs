module Truth.Core.Object.AsyncPush
    ( asyncPushObject
    , asyncPushWithinObject
    ,protectObject
    ) where

import Control.Concurrent.STM
import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.AutoClose
import Truth.Core.Object.Object
import Truth.Core.Read

data VarState
    = VSEmpty
    | VSDo (IO ())
    | VSDone

actionThread :: OpenClose (IO () -> IO ())
actionThread = MkOpenClose $ do
    actionVar :: TVar VarState <- newTVarIO $ VSEmpty
    let
        threadDo :: IO ()
        threadDo = do
            maction <-
                atomically $ do
                    vs <- readTVar actionVar
                    case vs of
                        VSDone -> do
                            writeTVar actionVar $ VSEmpty
                            return Nothing
                        VSEmpty -> mzero
                        VSDo action -> do
                            writeTVar actionVar $ VSEmpty
                            return $ Just action
            case maction of
                Just action -> do
                    action
                    threadDo
                Nothing -> return ()
        waitForEmpty :: STM ()
        waitForEmpty = do
            vs <- readTVar actionVar
            case vs of
                VSEmpty -> return ()
                _ -> mzero
        closer :: IO ()
        closer = do
            atomically $ do
                waitForEmpty
                writeTVar actionVar $ VSDone
            atomically waitForEmpty
        request :: IO () -> IO ()
        request action = atomically $ do
            vs <- readTVar actionVar
            case vs of
                VSDone -> return ()
                VSEmpty -> writeTVar actionVar $ VSDo action
                VSDo oldaction -> writeTVar actionVar $ VSDo $ oldaction >> action
    _ <- forkIO threadDo
    return (request,closer)

protectObject :: forall edit. Object edit -> OpenClose (Object edit)
protectObject (MkObject (MkUnliftIO run :: UnliftIO m) rd push) = MkOpenClose $ do
    var <- newMVar ()
    let
        run' :: forall a. m a -> IO a
        run' ma = withMVar var $ \() -> run ma
    return (MkObject (MkUnliftIO run') rd push,return ())

-- | pushes complete within an object run
asyncPushWithinObject :: forall edit. Object edit -> Object edit
asyncPushWithinObject (MkObject (MkUnliftIO run :: UnliftIO m) rd push) = let
    run' :: UnliftIO (ReaderT (IO () -> IO ()) m)
    run' =
        MkUnliftIO $ \rma -> run $ do
            (request,closer) <- liftIO $ unOpenClose actionThread
            a <- runReaderT rma request
            liftIO closer
            return a
    push' :: [edit] -> ReaderT (IO () -> IO ()) m (Maybe (ReaderT (IO () -> IO ()) m ()))
    push' edits = do
        maction <- lift $ push edits
        request <- ask
        return $
            case maction of
                Nothing -> Nothing
                Just action ->
                    Just $
                    lift $
                    liftIOWithUnlift $ \(MkUnliftIO unlift) -> request $ unlift action
    rd' :: MutableRead (ReaderT (IO () -> IO ()) m) (EditReader edit)
    rd' = liftMutableRead rd
    in MkObject run' rd' push'

-- | pushes outside object runs, as separate runs
asyncPushObject :: forall edit. OpenClose (Object edit) -> OpenClose (Object edit)
asyncPushObject ocObject = let
    ff :: Object edit -> (IO () -> IO ()) -> Object edit
    ff (MkObject (run :: UnliftIO m) rd push) request = let
        push' :: [edit] -> m (Maybe (m ()))
        push' edits = do
            maction <- push edits
            return $
                case maction of
                    Nothing -> Nothing
                    Just action ->
                        Just $
                        liftIO $ request $ runUnliftIO run action
        in MkObject run rd push'
    in ff <$> ocObject <*> actionThread
