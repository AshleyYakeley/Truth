module Truth.Core.Object.Savable
    ( SaveActions(..)
    , saveBufferSubscriber
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.DeferActionT
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
        sbVar <- liftIO $ newMVar $ error "uninitialised save buffer"
        let
            initA :: Object (WholeEdit (EditSubject edit)) -> IO (editorB, Object edit, SaveActions)
            initA (MkObject (unliftA :: UnliftIO ma) readA pushA) =
                runUnliftIO unliftA $ do
                    firstBuf <- readA ReadWhole
                    mvarRun sbVar $ put $ MkSaveBuffer firstBuf False
                    let
                        runB :: UnliftIO (StateT (SaveBuffer (EditSubject edit)) (DeferActionT ma))
                        runB = composeUnliftIO (mvarUnlift sbVar) $ composeUnliftIO runDeferActionT unliftA
                        readB ::
                               MutableRead (StateT (SaveBuffer (EditSubject edit)) (DeferActionT ma)) (EditReader edit)
                        readB = mSubjectToMutableRead $ fmap saveBuffer get
                    rec
                        let
                            pushB ::
                                   [edit]
                                -> StateT (SaveBuffer (EditSubject edit)) (DeferActionT ma) (Maybe (StateT (SaveBuffer (EditSubject edit)) (DeferActionT ma) ()))
                            pushB edits =
                                return $
                                Just $ do
                                    newbuf <-
                                        mutableReadToSubject $
                                        applyEdits edits $
                                        mSubjectToMutableRead $ do
                                            MkSaveBuffer oldbuf _ <- get
                                            return oldbuf
                                    put (MkSaveBuffer newbuf True)
                                    lift $ deferActionT $ updateB edB objB edits
                            objB :: Object edit
                            objB = MkObject runB readB pushB
                        edB <- liftIO $ initB objB
                    let
                        saveAction :: IO Bool
                        saveAction =
                            runUnliftIO unliftA $ do
                                MkSaveBuffer buf _ <- mvarRun sbVar get
                                maction <- pushA [MkWholeEdit buf]
                                case maction of
                                    Nothing -> return False
                                    Just action -> do
                                        action
                                        mvarRun sbVar $ put $ MkSaveBuffer buf False
                                        return True
                        revertAction :: IO Bool
                        revertAction = do
                            edits <-
                                runUnliftIO unliftA $ do
                                    buf <- readA ReadWhole
                                    mvarRun sbVar $ put $ MkSaveBuffer buf False
                                    getReplaceEditsFromSubject buf
                            liftIO $ updateB edB objB edits
                            return False
                        saveActions :: SaveActions
                        saveActions =
                            MkSaveActions $
                            runUnliftIO unliftA $ do
                                MkSaveBuffer _ changed <- mvarRun sbVar get
                                return $
                                    if changed
                                        then Just (saveAction, revertAction)
                                        else Nothing
                        edA = (edB, objB, saveActions)
                    return edA
            updateA ::
                   (editorB, Object edit, SaveActions)
                -> Object (WholeEdit (EditSubject edit))
                -> [WholeEdit (EditSubject edit)]
                -> IO ()
            updateA (edB, objB, _) _ edits = do
                MkSaveBuffer oldbuffer changed <- mvarRun sbVar get
                if changed
                    then return ()
                    else do
                        newbuffer <- mutableReadToSubject $ applyEdits edits $ subjectToMutableRead oldbuffer
                        newedits <- getReplaceEditsFromSubject newbuffer
                        updateB edB objB newedits
                        mvarRun sbVar $ put $ MkSaveBuffer newbuffer False
        (edA, _, actionA) <- subscribe subA initA updateA
        let
            (edB, objB, saveActions) = edA
            actionB = (actionA, saveActions)
        return (edB, objB, actionB)
