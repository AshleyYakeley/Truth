module Truth.Core.Object.Savable
    ( SaveActions(..)
    , saveBufferSubscriber
    ) where

import Truth.Core.Edit
import Truth.Core.Import

import Truth.Core.Object.Object
import Truth.Core.Object.Subscriber
import Truth.Core.Read
import Truth.Core.Types.Whole

data SaveBuffer a = MkSaveBuffer
    { saveBuffer :: a
    , _saveBufferChanged :: Bool
    }

newtype SaveActions =
    MkSaveActions (IO (Maybe (IO Bool, IO Bool)))

saveBufferSubscriber ::
       forall edit action. FullEdit edit
    => Subscriber (WholeEdit (EditSubject edit)) action
    -> Subscriber edit (action, SaveActions)
saveBufferSubscriber subA =
    MkSubscriber $ \(initB :: Object edit -> IO editorB) updateB -> do
        sbVar <- newMVar $ error "uninitialised save buffer"
        let
            initA :: Object (WholeEdit (EditSubject edit)) -> IO (editorB, SaveActions)
            initA (MkObject (MkUnliftIO runA :: UnliftIO ma) readA pushA) =
                runA $ do
                    firstBuf <- readA ReadWhole
                    mvarRun sbVar $ put $ MkSaveBuffer firstBuf False
                    let
                        runB :: UnliftIO (StateT (SaveBuffer (EditSubject edit)) ma)
                        runB = MkUnliftIO $ runA . mvarRun sbVar
                        readB :: MutableRead (StateT (SaveBuffer (EditSubject edit)) ma) (EditReader edit)
                        readB = mSubjectToMutableRead $ fmap saveBuffer get
                    rec
                        let
                            pushB ::
                                   [edit]
                                -> StateT (SaveBuffer (EditSubject edit)) ma (Maybe (StateT (SaveBuffer (EditSubject edit)) ma ()))
                            pushB edits =
                                return $
                                Just $ do
                                    newbuf <-
                                        mutableReadToSubject $
                                        applyEdits edits $
                                        mSubjectToMutableRead $ do
                                            MkSaveBuffer oldbuf _ <- get
                                            return oldbuf
                                    put $ MkSaveBuffer newbuf True
                                    updateB edB (mSubjectToMutableRead $ fmap saveBuffer get) edits
                            objB :: Object edit
                            objB = MkObject runB readB pushB
                        edB <- liftIO $ initB objB
                    let
                        saveAction :: IO Bool
                        saveAction =
                            runA $ do
                                MkSaveBuffer buf _ <- mvarRun sbVar get
                                maction <- pushA [MkWholeEdit buf]
                                case maction of
                                    Nothing -> return False
                                    Just action -> do
                                        action
                                        mvarRun sbVar $ put $ MkSaveBuffer buf False
                                        return True
                        revertAction :: IO Bool
                        revertAction =
                            runA $ do
                                buf <- readA ReadWhole
                                mvarRun sbVar $ put $ MkSaveBuffer buf False
                                edits <- getReplaceEditsFromSubject buf
                                updateB edB (subjectToMutableRead buf) edits
                                return False
                        saveActions :: SaveActions
                        saveActions =
                            MkSaveActions $
                            runA $ do
                                MkSaveBuffer _ changed <- mvarRun sbVar get
                                return $
                                    if changed
                                        then Just (saveAction, revertAction)
                                        else Nothing
                        edA = (edB, saveActions)
                    return edA
            updateA ::
                   forall m. MonadUnliftIO m
                => (editorB, SaveActions)
                -> MutableRead m (WholeReader (EditSubject edit))
                -> [WholeEdit (EditSubject edit)]
                -> m ()
            updateA (edB, _) _ edits = do
                MkSaveBuffer oldbuffer changed <- mvarRun sbVar get
                if changed
                    then return ()
                    else do
                        newbuffer <- mutableReadToSubject $ applyEdits edits $ subjectToMutableRead oldbuffer
                        newedits <- getReplaceEditsFromSubject newbuffer
                        updateB edB (subjectToMutableRead newbuffer) newedits
                        mvarRun sbVar $ put $ MkSaveBuffer newbuffer False
        (edA, closerA, actionA) <- subscribe subA initA updateA
        let
            (edB, saveActions) = edA
            actionB = (actionA, saveActions)
            closerB = closerA -- add UI query here
        return (edB, closerB, actionB)
