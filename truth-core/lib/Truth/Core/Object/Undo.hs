module Truth.Core.Object.Undo
    ( UndoActions(..)
    , undoQueueSubscriber
    ) where

import Truth.Core.Edit
import Truth.Core.Import

import Truth.Core.Object.Object
import Truth.Core.Object.Subscriber
import Truth.Core.Read
import Truth.Debug.Object

type UndoEntry edit = ([edit], [edit])

makeUndoEntry ::
       (MonadIO m, InvertibleEdit edit) => MutableRead m (EditReader edit) -> [edit] -> m (Maybe (UndoEntry edit))
makeUndoEntry _ [] = return Nothing
makeUndoEntry mr edits = do
    unedits <- invertEdits edits mr
    return $ Just (edits, unedits)

data UndoQueue edit = MkUndoQueue
    { _uqUndoEdits :: [UndoEntry edit]
    , _uqRedoEdits :: [UndoEntry edit]
    }

updateUndoQueue ::
       (MonadIO m, InvertibleEdit edit) => MutableRead m (EditReader edit) -> [edit] -> StateT (UndoQueue edit) m ()
updateUndoQueue mr edits = traceBracket "updateUndoQueue" $ do
    mue <- lift $ makeUndoEntry mr edits
    case mue of
        Nothing -> traceBracket "updateUndoQueue: not undoable" $ return ()
        Just ue -> traceBracket "updateUndoQueue: undoable" $ do
            MkUndoQueue uq _ <- get
            put $ MkUndoQueue (ue : uq) []

data UndoActions = MkUndoActions
    { uaUndo :: IO ()
    , uaRedo :: IO ()
    }

undoQueueSubscriber ::
       forall edit actions. InvertibleEdit edit
    => Subscriber edit actions
    -> Subscriber edit (actions, UndoActions)
undoQueueSubscriber sub =
    MkSubscriber $ \(init :: Object edit -> IO editor) update -> do
        queueVar <- liftIO $ newMVar $ MkUndoQueue [] []
        let
            init' :: Object edit -> IO (editor, UndoActions)
            init' object@(MkObject (MkTransform runA :: UnliftIO ma) _ pushA) = do
                editor <- init object
                let
                    uaUndo :: IO ()
                    uaUndo =
                        traceBracket "undoQueueSubscriber.uaUndo:outside" $ mvarRun queueVar $ traceBracket "undoQueueSubscriber.uaUndo:inside" $ do
                            MkUndoQueue ues res <- get
                            case ues of
                                [] -> traceBracket "undoQueueSubscriber.uaUndo: no undoable" $ return () -- nothing to undo
                                (entry:ee) -> traceBracket "undoQueueSubscriber.uaUndo: undoable" $ do
                                    did <-
                                        lift $
                                        runA $ do
                                            maction <- pushA (snd entry)
                                            case maction of
                                                Just action -> do
                                                    action
                                                    return True
                                                Nothing -> return False
                                    if did
                                        then put $ MkUndoQueue ee (entry : res)
                                        else return ()
                    uaRedo :: IO ()
                    uaRedo =
                        mvarRun queueVar $ do
                            MkUndoQueue ues res <- get
                            case res of
                                [] -> return () -- nothing to redo
                                (entry:ee) -> do
                                    did <-
                                        lift $
                                        runA $ do
                                            maction <- pushA (fst entry)
                                            case maction of
                                                Just action -> do
                                                    action
                                                    return True
                                                Nothing -> return False
                                    if did
                                        then put $ MkUndoQueue (entry : ues) ee
                                        else return ()
                return (editor, MkUndoActions {..})
            update' :: (editor, UndoActions) -> Object edit -> [edit] -> IO ()
            update' (editor, _) object@MkObject {..} edits = traceBracket "undoQueueSubscriber.update" $ do
                update editor object edits
                _ <- do
                    MkUnlift du <- mvarRun queueVar $ getDiscardingUnlift
                    runTransform objRun $ du $ updateUndoQueue objRead edits -- discard changes to the queue on undo and redo edits
                return ()
        ((editor, undoActions), object, actions) <- subscribe sub init' update'
        return (editor, object, (actions, undoActions))
