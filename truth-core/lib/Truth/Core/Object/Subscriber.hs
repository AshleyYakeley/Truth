module Truth.Core.Object.Subscriber
    ( mapUpdates
    , ReceiveUpdatesM
    , ReceiveUpdates
    , ReceiveUpdatesT
    , mapReceiveUpdates
    , mapReceiveUpdatesT
    , Subscriber(..)
    , makeObjectSubscriber
    , liftIO
    , UpdatingObject
    , updatingObject
    , makeSharedSubscriber
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.DeferActionT
import Truth.Core.Object.Object
import Truth.Core.Object.Update
import Truth.Core.Read

data Subscriber edit = MkSubscriber
    { subObject :: Object edit
    , subscribe :: ([edit] -> IO ()) -> LifeCycle ()
    }

type UpdateStoreEntry edit = [edit] -> IO ()

type UpdateStore edit = Store (UpdateStoreEntry edit)

runUpdateStoreEntry :: [edit] -> StateT (UpdateStoreEntry edit) IO ()
runUpdateStoreEntry edits = do
    update <- get
    lift $ update edits

updateStore :: [edit] -> StateT (UpdateStore edit) IO ()
updateStore edits = traverseStoreStateT $ \_ -> runUpdateStoreEntry edits

type UpdatingObject edit a = ([edit] -> IO ()) -> LifeCycle (Object edit, a)

makeSharedSubscriber :: forall edit a. UpdatingObject edit a -> IO (Subscriber edit, a)
makeSharedSubscriber uobj = do
    var :: MVar (UpdateStore edit) <- newMVar emptyStore
    let
        updateP :: [edit] -> IO ()
        updateP edits = mvarRun var $ updateStore edits
    ((objectC, a), closerP) <- runLifeCycle $ uobj updateP
    let
        child :: Subscriber edit
        child =
            MkSubscriber objectC $ \updateC ->
                case objectC of
                    MkObject runC _ _ ->
                        MkLifeCycle $ do
                            key <- runTransform runC $ mvarRun var $ addStoreStateT updateC
                            let
                                closerC =
                                    runTransform runC $
                                    mvarRun var $ do
                                        deleteStoreStateT key
                                        newstore <- get
                                        if isEmptyStore newstore
                                            then liftIO closerP
                                            else return ()
                            return ((), closerC)
    return (child, a)

updatingObject :: forall edit. Bool -> Object edit -> UpdatingObject edit ()
updatingObject async (MkObject (run :: UnliftIO m) r e) update = do
    runAsync <-
        if async
            then asyncRunner update
            else return update
    return $ let
        run' :: UnliftIO (DeferActionT m)
        run' = composeUnliftTransformCommute runDeferActionT run
        r' :: MutableRead (DeferActionT m) (EditReader edit)
        r' = liftMutableRead r
        e' :: [edit] -> DeferActionT m (Maybe (DeferActionT m ()))
        e' edits = do
            maction <- lift $ e edits
            case maction of
                Nothing -> return Nothing
                Just action ->
                    return $
                    Just $ do
                        lift action
                        deferActionT $ runAsync edits
        in (MkObject run' r' e', ())

makeObjectSubscriber :: Bool -> Object edit -> IO (Subscriber edit)
makeObjectSubscriber async object = do
    (sub, ()) <- makeSharedSubscriber $ updatingObject async object
    return sub
