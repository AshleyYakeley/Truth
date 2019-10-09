module Truth.Core.Object.Undo
    ( UndoActions(..)
    , undoQueueSubscriber
    ) where

import Truth.Core.Edit
import Truth.Core.Import

import Truth.Core.Object.EditContext
import Truth.Core.Object.Object
import Truth.Core.Object.Run
import Truth.Core.Object.Subscriber
import Truth.Core.Read

-- fst is original edits, snd is undoing edits
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
updateUndoQueue mr edits = do
    mue <- lift $ makeUndoEntry mr edits
    case mue of
        Nothing -> return ()
        Just ue -> do
            MkUndoQueue uq _ <- get
            put $ MkUndoQueue (ue : uq) []

data UndoActions = MkUndoActions
    { uaUndo :: EditSource -> IO Bool
    , uaRedo :: EditSource -> IO Bool
    }

undoQueueSubscriber ::
       forall update. InvertibleEdit (UpdateEdit update)
    => Subscriber update
    -> IO (Subscriber update, UndoActions)
undoQueueSubscriber sub = do
    queueVar <- newMVar $ MkUndoQueue [] []
    MkRunnableIO (runP :: WIOFunction ma) (MkASubscriber (MkAnObject readP pushP) subscribeP) <- return sub
    let
        undoActions = let
            uaUndo :: EditSource -> IO Bool
            uaUndo esrc =
                mVarRun queueVar $ do
                    MkUndoQueue ues res <- get
                    case ues of
                        [] -> return False -- nothing to undo
                        (entry:ee) -> do
                            did <-
                                lift $
                                runWMFunction runP $ do
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
            uaRedo :: EditSource -> IO Bool
            uaRedo esrc =
                mVarRun queueVar $ do
                    MkUndoQueue ues res <- get
                    case res of
                        [] -> return False -- nothing to redo
                        (entry:ee) -> do
                            did <-
                                lift $
                                runWMFunction runP $ do
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
        pushC edits = do
            maction <- pushP edits
            return $
                case maction of
                    Just action ->
                        Just $ \esrc -> do
                            mVarRun queueVar $ updateUndoQueue readP edits
                            action esrc
                    Nothing -> Nothing
        subC = MkRunnableIO runP $ MkASubscriber (MkAnObject readP pushC) subscribeP
    return (subC, undoActions)
