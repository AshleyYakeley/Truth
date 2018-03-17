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
    , UserInterface(..)
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.Object.Update
import Truth.Core.Read
import Truth.Debug

newtype Subscriber edit actions = MkSubscriber
    { subscribe :: forall editor. (Object edit -> IO editor) -- initialise: provides read MutableEdit, initial allowed, write MutableEdit
                                       -> (editor -> ReceiveUpdates edit) -- receive: get updates (both others and from your mutableEdit calls)
                                           -> LifeCycle (editor, actions)
    }

subscribeLifeCycle ::
       Subscriber edit actions
    -> (Object edit -> LifeCycle editor)
    -> (editor -> ReceiveUpdates edit)
    -> LifeCycle (editor, actions)
subscribeLifeCycle sub init receive = do
    ((editor, closer), actions) <- subscribe sub (runLifeCycle . init) (\(editor, _) -> receive editor)
    lifeCycleClose closer
    return (editor, actions)

instance Functor (Subscriber edit) where
    fmap ab (MkSubscriber sub) =
        MkSubscriber $ \initialise receive -> do
            (editor, a) <- sub initialise receive
            return (editor, ab a)

newtype UpdateStoreEntry edit =
    MkStoreEntry (ReceiveUpdates edit)

type UpdateStore edit = Store (UpdateStoreEntry edit)

runUpdateStoreEntry :: MonadUnliftIO m => (ReceiveUpdatesM m edit -> m ()) -> StateT (UpdateStoreEntry edit) m ()
runUpdateStoreEntry call = do
    MkStoreEntry update <- get
    lift $ call update

runUpdateStore :: MonadUnliftIO m => (Key -> ReceiveUpdatesM m edit -> m ()) -> StateT (UpdateStore edit) m ()
runUpdateStore call = traverseStoreStateT $ \key -> (runUpdateStoreEntry $ call key) >> return ()

updateStore :: MonadUnliftIO m => MutableRead m (EditReader edit) -> [edit] -> StateT (UpdateStore edit) m ()
updateStore mutr edits = runUpdateStore $ \_ ff -> ff mutr edits

makeSharedSubscriber :: forall edit actions. Subscriber edit actions -> IO (Subscriber edit actions)
makeSharedSubscriber parent = do
    var <- newMVar emptyStore
    let
        initP :: Object edit -> IO (Object edit)
        initP objectP = return objectP
        updateP :: Object edit -> ReceiveUpdates edit
        updateP _ mutrP edits = mvarRun var $ updateStore mutrP edits
    ((objectC@(MkObject (MkUnliftIO runC) _ _), actions), closerP) <- runLifeCycle $ subscribe parent initP updateP
    let
        child :: Subscriber edit actions
        child =
            MkSubscriber $ \initC updateC ->
                MkLifeCycle $ do
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
                    return ((editorC, actions), closerC)
    return child

objectSubscriber :: LifeCycle (Object edit) -> Subscriber edit ()
objectSubscriber ocObject =
    MkSubscriber $ \initr update -> do
        MkObject run r e <- ocObject
        rec
            editor <-
                liftIO $
                initr $ let
                    e' edits = do
                        maction <- e edits
                        case maction of
                            Nothing -> return Nothing
                            Just action ->
                                return $
                                Just $ do
                                    traceBracket "objectSubscriber: action" $ action
                                    traceBracket "objectSubscriber: update" $ update editor r edits
                    in MkObject run r e'
        return (editor, ())

makeObjectSubscriber :: Object edit -> IO (Subscriber edit ())
makeObjectSubscriber object = makeSharedSubscriber $ objectSubscriber $ pure object

data UserInterface specifier actions = forall edit. MkUserInterface
    { userinterfaceSubscriber :: Subscriber edit actions
    , userinterfaceSpecifier :: specifier edit
    }
