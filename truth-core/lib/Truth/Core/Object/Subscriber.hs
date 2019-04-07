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
    , subscribe :: ([edit] -> EditSource -> IO ()) -> LifeCycle ()
    }

type UpdateStoreEntry edit = [edit] -> EditSource -> IO ()

type UpdateStore edit = Store (UpdateStoreEntry edit)

runUpdateStoreEntry :: [edit] -> EditSource -> StateT (UpdateStoreEntry edit) IO ()
runUpdateStoreEntry edits esrc = do
    update <- get
    lift $ update edits esrc

updateStore :: [edit] -> EditSource -> StateT (UpdateStore edit) IO ()
updateStore edits esrc = traverseStoreStateT $ \_ -> runUpdateStoreEntry edits esrc

type UpdatingObject edit a = ([edit] -> EditSource -> IO ()) -> LifeCycle (Object edit, a)

makeSharedSubscriber :: forall edit a. UpdatingObject edit a -> IO (Subscriber edit, a)
makeSharedSubscriber uobj = do
    var :: MVar (UpdateStore edit) <- newMVar emptyStore
    let
        updateP :: [edit] -> EditSource -> IO ()
        updateP edits esrc = mvarRun var $ updateStore edits esrc
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
            then asyncIORunner
            else return id
    return $ let
        run' :: UnliftIO (DeferActionT m)
        run' = composeUnliftTransformCommute runDeferActionT run
        r' :: MutableRead (DeferActionT m) (EditReader edit)
        r' = liftMutableRead r
        e' :: [edit] -> DeferActionT m (Maybe (EditSource -> DeferActionT m ()))
        e' edits = do
            maction <- lift $ e edits
            case maction of
                Nothing -> return Nothing
                Just action ->
                    return $
                    Just $ \esrc -> do
                        lift $ action esrc
                        deferActionT $ runAsync $ update edits esrc
        in (MkObject run' r' e', ())

makeObjectSubscriber :: Bool -> Object edit -> IO (Subscriber edit)
makeObjectSubscriber async object = do
    (sub, ()) <- makeSharedSubscriber $ updatingObject async object
    return sub
