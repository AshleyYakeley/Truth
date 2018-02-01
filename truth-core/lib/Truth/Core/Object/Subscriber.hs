module Truth.Core.Object.Subscriber
    ( Subscriber(..)
    , makeObjectSubscriber
    , liftIO
    , objectSubscriber
    , makeSharedSubscriber
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.Read

newtype Subscriber edit actions = MkSubscriber
    { subscribe :: forall editor. (Object edit -> IO editor) -- initialise: provides read MutableEdit, initial allowed, write MutableEdit
                                       -> (forall m. MonadUnliftIO m =>
                                                         editor -> MutableRead m (EditReader edit) -> [edit] -> m () -- receive: get updates (both others and from your mutableEdit calls)
                                           ) -> IO (editor, IO (), actions)
    }

instance Functor (Subscriber edit) where
    fmap ab (MkSubscriber sub) =
        MkSubscriber $ \initialise receive -> do
            (editor, cl, a) <- sub initialise receive
            return (editor, cl, ab a)

newtype UpdateStoreEntry edit =
    MkStoreEntry (forall m. MonadUnliftIO m =>
                                MutableRead m (EditReader edit) -> [edit] -> m ())

type UpdateStore edit = Store (UpdateStoreEntry edit)

runUpdateStoreEntry ::
       MonadUnliftIO m
    => ((MutableRead m (EditReader edit) -> [edit] -> m ()) -> m ())
    -> StateT (UpdateStoreEntry edit) m ()
runUpdateStoreEntry call = do
    MkStoreEntry update <- get
    lift $ call update

runUpdateStore ::
       MonadUnliftIO m
    => (Key -> (MutableRead m (EditReader edit) -> [edit] -> m ()) -> m ())
    -> StateT (UpdateStore edit) m ()
runUpdateStore call = traverseStoreStateT $ \key -> (runUpdateStoreEntry $ call key) >> return ()

updateStore :: MonadUnliftIO m => MutableRead m (EditReader edit) -> [edit] -> StateT (UpdateStore edit) m ()
updateStore mutr edits = runUpdateStore $ \_ ff -> ff mutr edits

makeSharedSubscriber :: forall edit actions. Subscriber edit actions -> IO (Subscriber edit actions)
makeSharedSubscriber parent = do
    var <- newMVar emptyStore
    let
        initP :: Object edit -> IO (Object edit)
        initP objectP = return objectP
        updateP ::
               forall m. MonadUnliftIO m
            => Object edit
            -> MutableRead m (EditReader edit)
            -> [edit]
            -> m ()
        updateP _ mutrP edits = mvarRun var $ updateStore mutrP edits
    (objectC@(MkObject (MkUnliftIO runC) _ _), closerP, actions) <- subscribe parent initP updateP
    let
        child :: Subscriber edit actions
        child =
            MkSubscriber $ \initC updateC -> do
                editorC <- initC objectC
                key <- runC $ mvarRun var $ addStoreStateT $ MkStoreEntry $ updateC editorC
                let
                    closerC =
                        runC $
                        mvarRun var $ do
                            deleteStoreStateT key
                            newstore <- get
                            if isEmptyStore newstore
                                then liftIO closerP
                                else return ()
                return (editorC, closerC, actions)
    return child

objectSubscriber :: Object edit -> Subscriber edit ()
objectSubscriber (MkObject run r e) =
    MkSubscriber $ \initr update -> do
        rec
            editor <-
                initr $ let
                    e' edits = do
                        maction <- e edits
                        case maction of
                            Nothing -> return Nothing
                            Just action ->
                                return $
                                Just $ do
                                    action
                                    update editor r edits
                    in MkObject run r e'
        return (editor, return (), ())

makeObjectSubscriber :: Object edit -> IO (Subscriber edit ())
makeObjectSubscriber object = makeSharedSubscriber $ objectSubscriber object
