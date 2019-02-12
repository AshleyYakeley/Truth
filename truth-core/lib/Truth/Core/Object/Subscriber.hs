module Truth.Core.Object.Subscriber
    ( mapUpdates
    , ReceiveUpdatesM
    , ReceiveUpdates
    , ReceiveUpdatesT
    , mapReceiveUpdates
    , mapReceiveUpdatesT
    , Subscriber(..)
    , subscribeLifeCycle
    , makeObjectSubscriber
    , liftIO
    , objectSubscriber
    , makeSharedSubscriber
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.DeferActionT
import Truth.Core.Object.Object
import Truth.Core.Object.Update
import Truth.Core.Read
import Truth.Debug

newtype Subscriber edit actions = MkSubscriber
    { subscribe :: forall editor.
                           (Object edit -> IO editor) -- initialise: provides read MutableEdit, initial allowed, write MutableEdit
                            -> (editor -> Object edit -> [edit] -> IO ()) -- receive: get updates (both others and from your mutableEdit calls)
                                -> LifeCycle (editor, Object edit, actions)
    }

subscribeLifeCycle ::
       Subscriber edit actions
    -> (Object edit -> LifeCycle editor)
    -> (editor -> Object edit -> [edit] -> IO ())
    -> LifeCycle (editor, Object edit, actions)
subscribeLifeCycle sub init receive = do
    ((editor, closer), object, actions) <- subscribe sub (runLifeCycle . init) (\(editor, _) -> receive editor)
    lifeCycleClose closer
    return (editor, object, actions)

instance Functor (Subscriber edit) where
    fmap ab (MkSubscriber sub) =
        MkSubscriber $ \initialise receive -> do
            (editor, object, a) <- sub initialise receive
            return (editor, object, ab a)

type UpdateStoreEntry edit = [edit] -> IO ()

type UpdateStore edit = Store (UpdateStoreEntry edit)

runUpdateStoreEntry :: [edit] -> StateT (UpdateStoreEntry edit) IO ()
runUpdateStoreEntry edits = do
    update <- get
    lift $ update edits

updateStore :: [edit] -> StateT (UpdateStore edit) IO ()
updateStore edits = traverseStoreStateT $ \_ -> runUpdateStoreEntry edits

makeSharedSubscriber :: forall edit actions. Subscriber edit actions -> IO (Subscriber edit actions)
makeSharedSubscriber parent = do
    var :: MVar (UpdateStore edit) <- newMVar emptyStore
    let
        initP :: Object edit -> IO ()
        initP _ = return ()
        updateP :: () -> Object edit -> [edit] -> IO ()
        updateP () _ edits = mvarRun var $ updateStore edits
    (((), objectC@(MkObject (MkTransform runC) _ _), actions), closerP) <- runLifeCycle $ subscribe parent initP updateP
    let
        child :: Subscriber edit actions
        child =
            MkSubscriber $ \initC updateC ->
                MkLifeCycle $ do
                    editorC <- initC objectC
                    key <- runC $ mvarRun var $ addStoreStateT $ updateC editorC objectC
                    let
                        closerC =
                            runC $
                            mvarRun var $ do
                                deleteStoreStateT key
                                newstore <- get
                                if isEmptyStore newstore
                                    then liftIO closerP
                                    else return ()
                    return ((editorC, objectC, actions), closerC)
    return child

objectSubscriber :: forall edit. LifeCycle (Object edit) -> Subscriber edit ()
objectSubscriber ocObject =
    MkSubscriber $ \initr update -> do
        MkObject (run :: UnliftIO m) r e <- ocObject
        rec
            let
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
                                traceBracket "objectSubscriber: action" $ lift action
                                deferActionT $ traceBracket "objectSubscriber: deferred: update" $ update editor objectC edits
                objectC = MkObject run' r' e'
            editor <- liftIO $ initr $ objectC
        return (editor, objectC, ())

makeObjectSubscriber :: Object edit -> IO (Subscriber edit ())
makeObjectSubscriber object = makeSharedSubscriber $ objectSubscriber $ pure object
