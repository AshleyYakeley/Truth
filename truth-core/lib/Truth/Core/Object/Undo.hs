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
    { uaUndo :: IO Bool
    , uaRedo :: IO Bool
    }

undoQueueSubscriber ::
       forall edit. InvertibleEdit edit
    => Subscriber edit
    -> IO (Subscriber edit, UndoActions)
undoQueueSubscriber sub = do
    queueVar <- newMVar $ MkUndoQueue [] []
    MkObject (runP :: UnliftIO ma) readP pushP <- return $ subObject sub
    let
        undoActions = let
            uaUndo :: IO Bool
            uaUndo =
                traceBracket "undoQueueSubscriber.uaUndo:outside" $ mvarRun queueVar $ traceBracket "undoQueueSubscriber.uaUndo:inside" $ do
                    MkUndoQueue ues res <- get
                    case ues of
                        [] -> traceBracket "undoQueueSubscriber.uaUndo: no undoable" $ return False -- nothing to undo
                        (entry:ee) -> traceBracket "undoQueueSubscriber.uaUndo: undoable" $ do
                            did <-
                                lift $
                                runTransform runP $ do
                                    maction <- pushP (snd entry)
                                    case maction of
                                        Just action -> traceBracket "undoQueueSubscriber.uaUndo: action" $ do
                                            action
                                            return True
                                        Nothing -> traceBracket "undoQueueSubscriber.uaUndo: no action" $ return False
                            if did
                                then do
                                    put $ MkUndoQueue ee (entry : res)
                                    return True
                                else return False
            uaRedo :: IO Bool
            uaRedo =
                mvarRun queueVar $ do
                    MkUndoQueue ues res <- get
                    case res of
                        [] -> return False -- nothing to redo
                        (entry:ee) -> do
                            did <-
                                lift $
                                runTransform runP $ do
                                    maction <- pushP (fst entry)
                                    case maction of
                                        Just action -> do
                                            action
                                            return True
                                        Nothing -> return False
                            if did
                                then do
                                    put $ MkUndoQueue (entry : ues) ee
                                    return True
                                else return False
            in MkUndoActions {..}
        pushC edits = do
            maction <- pushP edits
            return $
                fmap
                    (\action -> traceBracket "undoQueueSubscriber.push" $ do
                         action
                         mvarRun queueVar $ updateUndoQueue readP edits)
                    maction
        objC = MkObject runP readP pushC
        subC = MkSubscriber objC $ subscribe sub
    return (subC, undoActions)
