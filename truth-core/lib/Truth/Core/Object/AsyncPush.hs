module Truth.Core.Object.AsyncPush
    ( asyncPushObject
    ) where

import Control.Concurrent.STM
import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.Read

data VarState
    = VSEmpty
    | VSDo (IO ())
    | VSDone

asyncPushObject :: forall edit. Object edit -> Object edit
asyncPushObject (MkObject (MkUnliftIO run :: UnliftIO m) rd push) = let
    run' :: UnliftIO (ReaderT (TVar VarState) m)
    run' =
        MkUnliftIO $ \rma -> run $ do
            actionVar :: TVar VarState <- liftIO $ newTVarIO $ VSEmpty
            let
                pusher :: IO ()
                pusher = do
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
                            pusher
                        Nothing -> return ()
                waitForEmpty :: STM ()
                waitForEmpty = do
                    vs <- readTVar actionVar
                    case vs of
                        VSEmpty -> return ()
                        _ -> mzero
            _ <- liftIO $ forkIO pusher
            a <- runReaderT rma actionVar
            liftIO $ atomically $ do
                waitForEmpty
                writeTVar actionVar $ VSDone
            liftIO $ atomically waitForEmpty
            return a
    push' :: [edit] -> ReaderT (TVar VarState) m (Maybe (ReaderT (TVar VarState) m ()))
    push' edits = do
        maction <- lift $ push edits
        actionVar <- ask
        return $
            case maction of
                Nothing -> Nothing
                Just action ->
                    Just $
                    lift $
                    liftIOWithUnlift $ \(MkUnliftIO unlift) ->
                        atomically $ do
                            vs <- readTVar actionVar
                            case vs of
                                VSDone -> return ()
                                VSEmpty -> writeTVar actionVar $ VSDo $ unlift action
                                VSDo oldaction -> writeTVar actionVar $ VSDo $ oldaction >> unlift action
    rd' :: MutableRead (ReaderT (TVar VarState) m) (EditReader edit)
    rd' = liftMutableRead rd
    in MkObject run' rd' push'
