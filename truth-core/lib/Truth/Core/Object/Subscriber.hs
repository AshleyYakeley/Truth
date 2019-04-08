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
import Truth.Core.Object.EditContext
import Truth.Core.Object.Object
import Truth.Core.Object.Update
import Truth.Core.Read
import Truth.Debug

data Subscriber edit = MkSubscriber
    { subObject :: Object edit
    , subscribe :: ([edit] -> EditContext -> IO ()) -> LifeCycle ()
    }

type UpdateStoreEntry edit = [edit] -> EditContext -> IO ()

type UpdateStore edit = Store (UpdateStoreEntry edit)

runUpdateStoreEntry :: [edit] -> EditContext -> StateT (UpdateStoreEntry edit) IO ()
runUpdateStoreEntry edits ectxt = do
    update <- get
    lift $ update edits ectxt

updateStore :: [edit] -> EditContext -> StateT (UpdateStore edit) IO ()
updateStore edits ectxt = traverseStoreStateT $ \_ -> runUpdateStoreEntry edits ectxt

type UpdatingObject edit a = ([edit] -> EditSource -> IO ()) -> LifeCycle (Object edit, a)

getRunner :: Bool -> LifeCycle ((EditContext -> IO ()) -> EditSource -> IO ())
getRunner False = return $ \action editContextSource -> action $ MkEditContext {editContextAsync = False, ..}
getRunner True = do
    runAsync <- asyncIORunner
    return $ \action editContextSource -> runAsync $ action $ MkEditContext {editContextAsync = True, ..}

makeSharedSubscriber :: forall edit a. Bool -> UpdatingObject edit a -> IO (Subscriber edit, a)
makeSharedSubscriber async uobj = do
    var :: MVar (UpdateStore edit) <- newMVar emptyStore
    let
        updateP :: [edit] -> EditContext -> IO ()
        updateP edits ectxt = mvarRun var $ updateStore edits ectxt
    ((objectC, a), closerP) <-
        runLifeCycle $ do
            runAsync <- getRunner async
            uobj $ \edits -> runAsync $ updateP edits
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

updatingObject :: forall edit. Object edit -> UpdatingObject edit ()
updatingObject (MkObject (run :: UnliftIO m) r e) update =
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
                        traceBracket "objectSubscriber: action" $ lift $ action esrc
                        deferActionT $ traceBracket "objectSubscriber: deferred: update" $ update edits esrc
        in (MkObject run' r' e', ())

makeObjectSubscriber :: Bool -> Object edit -> IO (Subscriber edit)
makeObjectSubscriber async object = do
    (sub, ()) <- makeSharedSubscriber async $ updatingObject object
    return sub
