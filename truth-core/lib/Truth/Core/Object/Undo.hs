module Truth.Core.Object.Undo
    ( UndoActions(..)
    , undoQueueSubscriber
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.EditContext
import Truth.Core.Object.Object
import Truth.Core.Object.Subscriber
import Truth.Core.Read
import Truth.Core.Resource

-- fst is original edits, snd is undoing edits
type UndoEntry edit = (NonEmpty edit, NonEmpty edit)

makeUndoEntry ::
       (MonadIO m, InvertibleEdit edit) => Readable m (EditReader edit) -> NonEmpty edit -> m (Maybe (UndoEntry edit))
makeUndoEntry mr edits = do
    unedits <- invertEdits (toList edits) mr
    case nonEmpty unedits of
        Nothing -> return Nothing
        Just unedits' -> return $ Just (edits, unedits')

data UndoQueue edit = MkUndoQueue
    { _uqUndoEdits :: [UndoEntry edit]
    , _uqRedoEdits :: [UndoEntry edit]
    }

updateUndoQueue ::
       (MonadIO m, InvertibleEdit edit) => Readable m (EditReader edit) -> NonEmpty edit -> StateT (UndoQueue edit) m ()
updateUndoQueue mr edits = do
    mue <- lift $ makeUndoEntry mr edits
    case mue of
        Nothing -> return ()
        Just ue -> do
            MkUndoQueue uq _ <- get
            put $ MkUndoQueue (ue : uq) []

data UndoActions = MkUndoActions
    { uaUndo :: ResourceContext -> EditSource -> IO Bool
    , uaRedo :: ResourceContext -> EditSource -> IO Bool
    }

undoQueueSubscriber ::
       forall update. InvertibleEdit (UpdateEdit update)
    => Subscriber update
    -> IO (Subscriber update, UndoActions)
undoQueueSubscriber sub = do
    queueVar <- newMVar $ MkUndoQueue [] []
    MkResource rrP (MkASubscriber (MkAnObject readP pushP ctaskP) subscribeP utaskP) <- return sub
    let
        undoActions = let
            uaUndo :: ResourceContext -> EditSource -> IO Bool
            uaUndo rc esrc =
                mVarRun queueVar $ do
                    MkUndoQueue ues res <- get
                    case ues of
                        [] -> return False -- nothing to undo
                        (entry:ee) -> do
                            did <-
                                lift $
                                runResourceRunner rc rrP $ do
                                    maction <- pushP (snd entry)
                                    case maction of
                                        Just action -> do
                                            action esrc
                                            return True
                                        Nothing -> return False
                            if did
                                then do
                                    put $ MkUndoQueue ee (entry : res)
                                    return True
                                else return False
            uaRedo :: ResourceContext -> EditSource -> IO Bool
            uaRedo rc esrc =
                mVarRun queueVar $ do
                    MkUndoQueue ues res <- get
                    case res of
                        [] -> return False -- nothing to redo
                        (entry:ee) -> do
                            did <-
                                lift $
                                runResourceRunner rc rrP $ do
                                    maction <- pushP (fst entry)
                                    case maction of
                                        Just action -> do
                                            action esrc
                                            return True
                                        Nothing -> return False
                            if did
                                then do
                                    put $ MkUndoQueue (entry : ues) ee
                                    return True
                                else return False
            in MkUndoActions {..}
        pushC edits =
            case resourceRunnerStackUnliftDict @IO rrP of
                Dict -> do
                    maction <- pushP edits
                    return $
                        case maction of
                            Just action ->
                                Just $ \esrc -> do
                                    mVarRun queueVar $ updateUndoQueue readP edits
                                    action esrc
                            Nothing -> Nothing
        subC = MkResource rrP $ MkASubscriber (MkAnObject readP pushC ctaskP) subscribeP utaskP
        in return (subC, undoActions)
